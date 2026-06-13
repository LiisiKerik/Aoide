{-|
Keyboard playability and reductions.
-}
module Composition.Keyboard (
  Keyboard (..),
  Playability (..),
  Playable (..),
  keyboard_playable,
  notes_to_keyboard,
  parse_playability,
  reduction) where
  import Composition.Errors
  import Composition.Notation
  import Composition.Notes
  import Composition.Score
  import Composition.Theory
  import Control.Lens.Combinators hiding (parts)
  import Control.Monad.Except
  import Control.Monad.Trans.Except
  import Data.Char
  import Data.Foldable as Foldable
  import Data.Functor
  import Data.List as List
  import Data.Map.Strict as Map hiding (keys)
  import Data.Maybe as Maybe
  import Data.Ord
  import Data.Set as Set
  import Parser.Files
  import Parser.Locations
  import Parser.Parser
  import Parser.Utilities
  data Char_class =
    Delimiter_char Token | Invalid_char | Keyword_char Char | Newline_char | Nonzero_nat_char Char | Whitespace_char | Zero_char
  -- | Data structure for storing the left and right hand part of a chord or score part.
  data Keyboard t = Keyboard t t
  data Notes_in = Notes_in (Set (Note Pitched)) (Set (Note Pitched))
  data Notes_in' = Notes_in' (Set (Note Pitched)) | Tie_in' (Set (Note Pitched))
  data Notes_out = Rest_out | Notes_out (Set (Note Pitched)) | Tie_out
  type Parser = Parser' Token Error
  -- | Data structure for describing playability. It describes which note combinations can be played with the right hand and
  -- which note combinations should be discarded even when spread between two hands.
  data Playability = Playability {right_hand_playables :: [Playable], discard :: [[Semitones]]}
  -- | Data structure for describing playable note combinations. For example, @Playable {semitones = [15], starting_keys =
  -- [3 10]}@ means that two notes with an interval of 15 semitones are playable only starting from D#/Eb and A#/Bb.
  data Playable = Playable {semitones :: [Semitones], starting_keys :: Set Int}
  data Token =
    Discard_token |
    Eq_token |
    Left_curly_bracket_token |
    Left_square_bracket_token |
    Playable_token |
    Playability_token |
    Positive_int_token Int |
    Right_hand_playables_token |
    Right_curly_bracket_token |
    Right_square_bracket_token |
    Semitones_token |
    Starting_keys_token |
    Zero_token
  type Tokeniser = Tokeniser' Char_class Token Error
  instance Applicative Keyboard where
    Keyboard f g <*> Keyboard left_hand right_hand = Keyboard (f left_hand) (g right_hand)
    pure x = Keyboard x x
  deriving instance Eq Char_class
  deriving instance Eq Token
  instance Foldable Keyboard where
    foldr f x (Keyboard left_hand right_hand) = f left_hand (f right_hand x)
  instance Functor Keyboard where
    fmap f (Keyboard left_hand right_hand) = Keyboard (f left_hand) (f right_hand)
  deriving instance (Show t) => Show (Keyboard t)
  deriving instance Show Playability
  deriving instance Show Playable
  instance Traversable Keyboard where
    traverse f (Keyboard left_hand right_hand) = Keyboard <$> f left_hand <*> f right_hand
  all_notes :: [Note Pitched] -> [[Note Pitched]]
  all_notes notes =
    case notes of
      [] -> [[]]
      note : notes' -> [id, (:) note] <*> all_notes notes'
  best :: (Eq u) => (t -> u) -> ([u] -> u) -> [t] -> [t]
  best metric best_metric x = List.filter (\ y -> best_metric (metric <$> x) == metric y) x
  best_keyboard ::
    Keyboard (Set (Note Pitched)) -> Set (Note Pitched) -> [Keyboard (Set (Note Pitched))] -> Keyboard (Set (Note Pitched))
  best_keyboard old_keyboard init_notes =
    (
      head <$>
      best right_hand_note_count maximum <$>
      best narrower_hand_span minimum <$>
      best wider_hand_note_count minimum <$>
      best wider_hand_span minimum <$>
      best crosses_middle_c minimum <$>
      best (hands_cross_compared_to_old old_keyboard) minimum <$>
      best (most_relevant_notes init_notes) maximum)
  classify_char :: Char -> Char_class
  classify_char c =
    case c of
      '\n' -> Newline_char
      ' ' -> Whitespace_char
      _ | elem c "'_" || isLetter c -> Keyword_char c
      '0' -> Zero_char
      _ | isDigit c && c /= '0' -> Nonzero_nat_char c
      '=' -> Delimiter_char Eq_token
      '[' -> Delimiter_char Left_square_bracket_token
      ']' -> Delimiter_char Right_square_bracket_token
      '{' -> Delimiter_char Left_curly_bracket_token
      '}' -> Delimiter_char Right_curly_bracket_token
      _ -> Invalid_char
  construct_keys :: [Int] -> Either (Location -> Error) (Set Int)
  construct_keys keys =
    case construct_set keys of
      Nothing -> Left Duplicate_keys
      Just keys' -> Right keys'
  construct_keyword :: String -> Either (Location -> Error) Token
  construct_keyword keyword =
    case keyword of
      "Playability" -> Right Playability_token
      "Playable" -> Right Playable_token
      "discard" -> Right Discard_token
      "right_hand_playables" -> Right Right_hand_playables_token
      "semitones" -> Right Semitones_token
      "starting_keys" -> Right Starting_keys_token
      _ -> Left (Invalid_keyword_playability keyword)
  continuing_notes_and_event ::
    Set (Note Pitched) -> Length_fraction -> Set (Note Pitched) -> Set (Note Pitched) -> (Set (Note Pitched), Event' Notes_out)
  continuing_notes_and_event ties len old_notes new_notes =
    let
      (continuing_notes, notes_out) =
        case Set.elems new_notes of
          [] ->
            case Set.elems (Set.intersection old_notes ties) of
              [] -> (Set.empty, Rest_out)
              _ : _ -> (old_notes, Tie_out)
          _ : _ -> (new_notes, Notes_out new_notes) in
      (continuing_notes, Event' notes_out len)
  crosses_middle_c :: Keyboard (Set (Note Pitched)) -> Int
  crosses_middle_c (Keyboard left_hand_notes right_hand_notes) =
    crosses_middle_c' id Set.findMax left_hand_notes `max` crosses_middle_c' flip Set.findMin right_hand_notes
  crosses_middle_c' ::
    (
      ((Note Pitched -> Note Pitched -> Int) -> Note Pitched -> Note Pitched -> Int) ->
      (Set (Note Pitched) -> Note Pitched) ->
      Set (Note Pitched) ->
      Int)
  crosses_middle_c' order_arguments innermost_note notes =
    order_arguments distance_in_semitones (Pitched_note 4 C) (innermost_note (Set.insert (Pitched_note 4 C) notes))
  delimiter_char :: Char_class -> Maybe Token
  delimiter_char char_class =
    case char_class of
      Delimiter_char token -> Just token
      _ -> Nothing
  events_out_to_events :: [Event' Notes_out] -> [Event_fraction Pitched]
  events_out_to_events events =
    case events of
      [] -> []
      Event' notes len : events' -> events_out_to_events' (Event' (notes_out_to_set notes) len) events'
  events_out_to_events' :: Event_fraction Pitched -> [Event' Notes_out] -> [Event_fraction Pitched]
  events_out_to_events' event@(Event' notes_0 len_0) events =
    case events of
      [] -> [event]
      Event' maybe_notes_1 len_1 : events' ->
        case maybe_notes_1 of
          Rest_out ->
            case Set.elems notes_0 of
              [] -> events_out_to_events' (Event' Set.empty (len_0 + len_1)) events'
              _ : _ -> event : events_out_to_events' (Event' Set.empty len_1) events'
          Notes_out notes_1 -> event : events_out_to_events' (Event' notes_1 len_1) events'
          Tie_out -> events_out_to_events' (Event' notes_0 (len_0 + len_1)) events'
  events_to_events_in :: [[Event' (Set (Note Pitched))]] -> [Event' Notes_in]
  events_to_events_in events = events_to_events_in' ((<$>) (over #event_notes Notes_in') <$> events)
  events_to_events_in' :: [[Event' Notes_in']] -> [Event' Notes_in]
  events_to_events_in' events =
    case () of
      () | all Foldable.null events -> []
      () ->
        let
          len = minimum (view #event_length <$> head <$> events)
          heads_and_tails = head_and_tail len <$> events in
          (
            Event' (Foldable.foldr union_notes_in (Notes_in Set.empty Set.empty) (fst <$> heads_and_tails)) len :
            events_to_events_in' (snd <$> heads_and_tails))
  hand_span :: Set (Note Pitched) -> Int
  hand_span notes =
    case Set.elems notes of
      [] -> -1
      _ -> distance_in_semitones (minimum notes) (maximum notes)
  hands_cross_compared_to_old :: Keyboard (Set (Note Pitched)) -> Keyboard (Set (Note Pitched)) -> Int
  hands_cross_compared_to_old
    (Keyboard old_left_hand_notes old_right_hand_notes)
    (Keyboard new_left_hand_notes new_right_hand_notes) =
    (
      hands_crossed (Keyboard old_left_hand_notes new_right_hand_notes) `max`
      hands_crossed (Keyboard new_left_hand_notes old_right_hand_notes))
  hands_crossed :: Keyboard (Set (Note Pitched)) -> Int
  hands_crossed (Keyboard left_hand_notes right_hand_notes) =
    case (Set.elems left_hand_notes, Set.elems right_hand_notes) of
      (_ : _, _ : _) -> -1 `max` distance_in_semitones (Set.findMin right_hand_notes) (Set.findMax left_hand_notes)
      _ -> -1
  hands_not_crossed :: Keyboard (Set (Note Pitched)) -> Bool
  hands_not_crossed keyboard = -1 == hands_crossed keyboard
  head_and_tail :: Length_fraction -> [Event' Notes_in'] -> (Notes_in, [Event' Notes_in'])
  head_and_tail len events =
    case events of
      [] -> undefined
      Event' notes len' : events' ->
        (
          transform_notes_in notes,
          case () of
            () | len == len' -> events'
            () -> Event' (notes_in_to_tie notes) (len' - len) : events')
  -- | Check if this configuration is playable on keyboard.
  keyboard_playable :: Playability -> Keyboard (Set (Note Pitched)) -> Bool
  keyboard_playable (Playability {right_hand_playables, discard}) keyboard@(Keyboard left_hand_notes right_hand_notes) =
    (
      hands_not_crossed keyboard &&
      no_discard discard (Set.elems (Set.union left_hand_notes right_hand_notes)) &&
      keyboard_playable_help right_hand_playables keyboard)
  keyboard_playable_help :: [Playable] -> Keyboard (Set (Note Pitched)) -> Bool
  keyboard_playable_help playables (Keyboard left_hand_notes right_hand_notes) =
    notes_playable (mirror_playables playables) left_hand_notes && notes_playable playables right_hand_notes
  keyword_char :: Char_class -> Maybe Char
  keyword_char char_class =
    case char_class of
      Keyword_char c -> Just c
      _ -> Nothing
  mirror_key :: Int -> Int
  mirror_key key = mod (4 - key) 12
  mirror_playable :: Playable -> Playable
  mirror_playable (Playable semitones keys) = Playable (reverse semitones) (Set.fromList (mirror_key <$> Set.elems keys))
  mirror_playables :: [Playable] -> [Playable]
  mirror_playables = (<$>) mirror_playable
  most_relevant_notes :: Set (Note Pitched) -> Keyboard (Set (Note Pitched)) -> Int
  most_relevant_notes init_notes (Keyboard l r) = most_relevant_notes' init_notes True (Set.union l r)
  most_relevant_notes' :: Set (Note Pitched) -> Bool -> Set (Note Pitched) -> Int
  most_relevant_notes' init_notes l_or_h notes =
    case Set.null init_notes of
      True -> 0
      False ->
        let
          relevant_note =
            case l_or_h of
              False -> Set.findMin init_notes
              True -> Set.findMax init_notes
          init_notes' = Set.delete relevant_note init_notes
          bit =
            case Set.member relevant_note notes of
              False -> 0
              True -> 2 ^ Set.size init_notes' in
          bit + most_relevant_notes' init_notes' (not l_or_h) notes
  nat_char :: Char_class -> Maybe Char
  nat_char char_class =
    case char_class of
      Zero_char -> Just '0'
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  narrower_hand_span :: Keyboard (Set (Note Pitched)) -> Int
  narrower_hand_span keyboard = minimum (hand_span <$> keyboard)
  next_location :: Char_class -> Location -> Location
  next_location char_class =
    case char_class of
      Newline_char -> next_line
      _ -> next_char
  no_discard :: [[Int]] -> [Note Pitched] -> Bool
  no_discard discard notes = and (no_discard_1 <$> discard <*> all_notes notes)
  no_discard_1 :: [Int] -> [Note Pitched] -> Bool
  no_discard_1 discard my_notes =
    case my_notes of
      [] -> True
      note : my_notes' -> discard /= notes_to_semitones note my_notes'
  nonzero_nat_char :: Char_class -> Maybe Char
  nonzero_nat_char char_class =
    case char_class of
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  notes_in_to_tie :: Notes_in' -> Notes_in'
  notes_in_to_tie maybe_notes =
    case maybe_notes of
      Notes_in' notes -> Tie_in' notes
      Tie_in' notes -> Tie_in' notes
  notes_out_to_set :: Notes_out -> Set (Note Pitched)
  notes_out_to_set maybe_notes =
    case maybe_notes of
      Rest_out -> Set.empty
      Notes_out notes -> notes
      Tie_out -> Set.empty
  notes_playable :: [Playable] -> Set (Note Pitched) -> Bool
  notes_playable playables notes =
    case Set.elems notes of
      [] -> True
      note@(Pitched_note _ note_name) : notes' ->
        any (notes_playable' (notes_to_semitones note notes') (semitones_from_c note_name)) playables
  notes_playable' :: [Int] -> Int -> Playable -> Bool
  notes_playable' semitones' starting_key (Playable semitones starting_keys) =
    semitones == semitones' && any ((==) starting_key) starting_keys
  notes_to_semitones :: Note Pitched -> [Note Pitched] -> [Int]
  notes_to_semitones note_0 notes =
    case notes of
      [] -> []
      note_1 : notes' -> distance_in_semitones note_0 note_1 : notes_to_semitones note_1 notes'
  -- | Check if the set of notes is playable on keyboard.
  notes_to_keyboard :: Playability -> Set (Note Pitched) -> Bool
  notes_to_keyboard playability notes =
    not (Foldable.null (List.filter (keyboard_playable playability) (split_notes_between_hands notes)))
  parse_curly_brackets :: Parser t -> Parser t
  parse_curly_brackets = parse_brackets Left_curly_bracket_token Right_curly_bracket_token
  parse_eq :: Parser ()
  parse_eq = parse_token Eq_token
  parse_field :: Token -> Parser t -> Parser t
  parse_field name parse_t =
    do
      parse_token name
      parse_eq
      parse_t
  parse_key :: Parser Int
  parse_key = filter_parser ((>=) 11) Key_is_out_of_range parse_nat
  parse_keys :: Parser (Set Int)
  parse_keys = fmap_filter_parser construct_keys (parse_list' parse_key)
  parse_list' :: Parser t -> Parser [t]
  parse_list' parse_t = parse_square_brackets (parse_many parse_t)
  parse_nat :: Parser Int
  parse_nat = parse_zero <+> parse_positive_int
  parse_playable :: Parser Playable
  parse_playable =
    parse_struct
      Playable_token
      (do
        semitones <- parse_field Semitones_token parse_semitones
        starting_keys <- parse_field Starting_keys_token parse_keys
        return Playable {semitones, starting_keys})
  -- | Parse keyboard playability file.
  parse_playability :: File_path -> ExceptT Error IO Playability
  parse_playability file_path =
    do
      playability <- read_file "key" readFile File_error file_path
      except (fromJust (parse' classify_char next_location tokenise parse_playability' Parse_error_playability playability))
  parse_playability' :: Parser Playability
  parse_playability' =
    parse_struct
      Playability_token
      (do
        right_hand_playables <- parse_field Right_hand_playables_token (parse_list' parse_playable)
        discard <- parse_field Discard_token (parse_list' parse_semitones)
        return Playability {right_hand_playables, discard})
  parse_positive_int :: Parser Int
  parse_positive_int = parse_token' positive_int_token
  parse_semitones :: Parser [Int]
  parse_semitones = parse_list' parse_nat
  parse_square_brackets :: Parser t -> Parser t
  parse_square_brackets = parse_brackets Left_square_bracket_token Right_square_bracket_token
  parse_struct :: Token -> Parser t -> Parser t
  parse_struct name parse_fields =
    do
      parse_token name
      parse_curly_brackets parse_fields
  parse_zero :: Parser Int
  parse_zero =
    do
      parse_token Zero_token
      return 0
  pitched_events :: Pitched_or_unpitched Track -> Maybe [Event_fraction Pitched]
  pitched_events = pitched_or_unpitched (\ (Track {events}) -> Just events) (\ _ -> Nothing)
  positive_int_token :: Token -> Maybe Int
  positive_int_token token =
    case token of
      Positive_int_token i -> Just i
      _ -> Nothing
  -- | Keyboard reduction. You have to provide playability data, optional instrument score header field and a MIDI instrument
  -- code.
  reduction :: Playability -> Map Header_field String -> MIDI_instrument -> Score -> Either Error Score
  reduction (Playability {right_hand_playables, discard}) header midi_instrument (Score {parts}) =
    do
      keyboard_parts <- traverse keyboard_part parts
      Right (Score {header, parts = keyboard_parts}) where
    all_keyboards :: Set (Note Pitched) -> [Keyboard (Set (Note Pitched))]
    all_keyboards notes =
      List.filter
        (keyboard_playable_help right_hand_playables)
        (
          Set.fromList <$> List.filter (no_discard discard) (all_notes (Set.elems notes)) >>=
          split_notes_between_hands)
    events_to_keyboard :: Keyboard (Set (Note Pitched)) -> [Event' Notes_in] -> Keyboard [Event' Notes_out]
    events_to_keyboard old_keyboard events =
      case events of
        [] -> pure []
        Event' (Notes_in new_notes ties) len : events' ->
          let
            new_keyboard = best_keyboard old_keyboard new_notes (all_keyboards new_notes)
            continuing_notes_and_event_keyboard = continuing_notes_and_event ties len <$> old_keyboard <*> new_keyboard in
            (
              (:) <$>
              (snd <$> continuing_notes_and_event_keyboard) <*>
              events_to_keyboard (fst <$> continuing_notes_and_event_keyboard) events')
    keyboard_part :: Part -> Either Error Part
    keyboard_part (Part {title = part_title, key, time, initial_position, tempo, stave_groups}) =
      do
        let tracks = remove_notational_information stave_groups
        _ <- tracks_length tracks
        Keyboard left_hand_track right_hand_track <-
          traverse
            (add_notational_information time initial_position)
            (
              events_out_to_events <$>
              events_to_keyboard (pure Set.empty) (events_to_events_in (Maybe.mapMaybe pitched_events tracks)))
        Right
          (Part {
            title = part_title,
            key,
            time,
            initial_position,
            tempo,
            stave_groups =
              [
                [
                  Pitched
                    (Instrument_and_staves {
                      instrument_name = "",
                      short_instrument_name = "",
                      midi_instrument,
                      velocity = max_velocity,
                      staves =
                        [
                          Stave {
                            clef = Pitched_clef {clef_name = Treble, transposition = 0},
                            tracks = One_track right_hand_track},
                          Stave {
                            clef = Pitched_clef {clef_name = Bass, transposition = 0},
                            tracks = One_track left_hand_track}]})]]})
  right_hand_note_count :: Keyboard (Set (Note Pitched)) -> Int
  right_hand_note_count (Keyboard _ right_hand_notes) = Set.size right_hand_notes
  split_notes_between_hands :: Set (Note Pitched) -> [Keyboard (Set (Note Pitched))]
  split_notes_between_hands notes = Keyboard Set.empty notes : split_notes_between_hands' Set.empty (Set.elems notes)
  split_notes_between_hands' :: Set (Note Pitched) -> [Note Pitched] -> [Keyboard (Set (Note Pitched))]
  split_notes_between_hands' left_hand_notes notes =
    case notes of
      [] -> []
      note : notes' ->
        let
          left_hand_notes' = Set.insert note left_hand_notes in
          Keyboard left_hand_notes' (Set.fromList notes') : split_notes_between_hands' left_hand_notes' notes'
  tokenise :: Tokeniser ()
  tokenise = void (parse_many tokenise_1)
  tokenise_1 :: Tokeniser ()
  tokenise_1 = tokenise_delimiter <+> tokenise_keyword <+> tokenise_nat <+> tokenise_newline <+> tokenise_whitespace
  tokenise_delimiter :: Tokeniser ()
  tokenise_delimiter = add_token (parse_token' delimiter_char)
  tokenise_keyword :: Tokeniser ()
  tokenise_keyword = add_token (fmap_filter_parser construct_keyword (parse_some (parse_token' keyword_char)))
  tokenise_nat :: Tokeniser ()
  tokenise_nat = add_token (tokenise_zero <+> tokenise_positive_int)
  tokenise_newline :: Tokeniser ()
  tokenise_newline = parse_token Newline_char
  tokenise_positive_int :: Tokeniser Token
  tokenise_positive_int =
    Positive_int_token <$> read <$> ((:) <$> parse_token' nonzero_nat_char <*> parse_many (parse_token' nat_char))
  tokenise_whitespace :: Tokeniser ()
  tokenise_whitespace = parse_token Whitespace_char
  tokenise_zero :: Tokeniser Token
  tokenise_zero =
    do
      parse_token Zero_char
      return Zero_token
  transform_notes_in :: Notes_in' -> Notes_in
  transform_notes_in maybe_notes =
    case maybe_notes of
      Notes_in' notes -> Notes_in notes Set.empty
      Tie_in' notes -> Notes_in Set.empty notes
  union_notes_in :: Notes_in -> Notes_in -> Notes_in
  union_notes_in (Notes_in notes_0 ties_0) (Notes_in notes_1 ties_1) =
    Notes_in (Set.union notes_0 notes_1) (Set.union ties_0 ties_1)
  wider_hand_span :: Keyboard (Set (Note Pitched)) -> Int
  wider_hand_span keyboard = maximum (hand_span <$> keyboard)
  wider_hand_note_count :: Keyboard (Set (Note Pitched)) -> Int
  wider_hand_note_count keyboard = Set.size (maximumBy (comparing hand_span) keyboard)