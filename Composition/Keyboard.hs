{-| ---
Description: Functions that deal with keyboard instruments. ---
 ---
* Checking whether a set of notes is playable on the keyboard ---
* Keyboard transcriptions ---
-} ---
module Composition.Keyboard (Keyboard (..), Playable (..), is_playable, keyboard, parse_playable, set_is_playable) where ---
  import Composition.Errors ---
  import Composition.Notes ---
  import Composition.Score ---
  import Composition.Theory ---
  import Control.Lens.Combinators hiding (parts) ---
  import Control.Monad.Except ---
  import Control.Monad.Trans.Except ---
  import Data.Char ---
  import Data.Foldable as Foldable ---
  import Data.Functor ---
  import Data.List as List ---
  import Data.Map.Strict as Map hiding (keys) ---
  import Data.Maybe as Maybe ---
  import Data.Ord ---
  import Data.Set as Set ---
  import Parser.Files ---
  import Parser.Locations ---
  import Parser.Parser ---
  import Parser.Utilities ---
  data Char_class = Delimiter_char Token | Invalid_char | Nonzero_nat_char Char | Whitespace_char | Zero_char ---
  -- | Data structure for storing the left and right hand part of a chord or a piece. ---
  data Keyboard t = Keyboard t t ---
  data Notes_in = Notes_in (Set (Note Pitched)) (Set (Note Pitched)) ---
  data Notes_in' = Notes_in' (Set (Note Pitched)) | Tie_in' (Set (Note Pitched)) ---
  data Notes_out = Rest_out | Notes_out (Set (Note Pitched)) | Tie_out ---
  type Parser = Parser' Token Error ---
  -- | Data structure for describing playable note combinations. For example, [15] [3 10] means that two notes with an interval ---
  -- of 15 semitones are playable only starting from D#/Eb and A#/Bb. ---
  data Playable = Playable {intervals :: [Int], starting_keys :: Set Int} ---
  data Token = Left_square_bracket_token | Newline_token | Positive_int_token Int | Right_square_bracket_token | Zero_token ---
  type Tokeniser = Tokeniser' Char_class Token Error ---
  instance Applicative Keyboard where ---
    Keyboard f g <*> Keyboard left_hand right_hand = Keyboard (f left_hand) (g right_hand) ---
    pure x = Keyboard x x ---
  deriving instance Eq Char_class ---
  deriving instance Eq Token ---
  instance Foldable Keyboard where ---
    foldr f x (Keyboard left_hand right_hand) = f left_hand (f right_hand x) ---
  instance Functor Keyboard where ---
    fmap f (Keyboard left_hand right_hand) = Keyboard (f left_hand) (f right_hand) ---
  instance Traversable Keyboard where ---
    traverse f (Keyboard left_hand right_hand) = Keyboard <$> f left_hand <*> f right_hand ---
  classify_char :: Char -> Char_class ---
  classify_char c = ---
    case c of ---
      '\n' -> Delimiter_char Newline_token ---
      ' ' -> Whitespace_char ---
      '0' -> Zero_char ---
      _ | isDigit c && c /= '0' -> Nonzero_nat_char c ---
      '[' -> Delimiter_char Left_square_bracket_token ---
      ']' -> Delimiter_char Right_square_bracket_token ---
      _ -> Invalid_char ---
  construct_keys :: [Int] -> Either (Location -> Error) (Set Int) ---
  construct_keys keys = ---
    case construct_set keys of ---
      Nothing -> Left Duplicate_keys ---
      Just keys' -> Right keys' ---
  crossing_middle_c :: Keyboard (Set (Note Pitched)) -> Int ---
  crossing_middle_c (Keyboard l r) = crossing_middle_c' id Set.findMax l `max` crossing_middle_c' flip Set.findMin r ---
  crossing_middle_c' :: ---
    ( ---
      ((Note Pitched -> Note Pitched -> Int) -> Note Pitched -> Note Pitched -> Int) -> ---
      (Set (Note Pitched) -> Note Pitched) -> ---
      Set (Note Pitched) -> ---
      Int) ---
  crossing_middle_c' maybeflip aggr notes = ---
    (maybeflip distance_in_semitones (Pitched_note 4 C)) (aggr (Set.insert (Pitched_note 4 C) notes)) ---
  delimiter_char :: Char_class -> Maybe Token ---
  delimiter_char char_class = ---
    case char_class of ---
      Delimiter_char token -> Just token ---
      _ -> Nothing ---
  events_to_events_in :: [[Event' (Set (Note Pitched))]] -> [Event' Notes_in] ---
  events_to_events_in events = events_to_events_in' ((<$>) (over #event_notes Notes_in') <$> events) ---
  events_to_events_in' :: [[Event' (Notes_in')]] -> [Event' Notes_in] ---
  events_to_events_in' events = ---
    case () of ---
      () | all Foldable.null events -> [] ---
      () -> ---
        let ---
          len = minimum (view #event_length <$> head <$> events) ---
          heads_and_tails = head_and_tail len <$> events in ---
          ( ---
            Event' (Foldable.foldr join_notes (Notes_in Set.empty Set.empty) (fst <$> heads_and_tails)) len : ---
            events_to_events_in' (snd <$> heads_and_tails)) ---
  filter_by_key :: (Eq t) => ([t] -> t) -> (u -> t) -> [u] -> [u] ---
  filter_by_key fold_keys compute_key x = List.filter (\ y -> fold_keys (compute_key <$> x) == compute_key y) x ---
  gen_hands :: [Playable] -> [Note Pitched] -> [Set (Note Pitched)] ---
  gen_hands playable notes = ---
    Set.empty : gen_hands' Set.empty notes where ---
    gen_hands' :: Set (Note Pitched) -> [Note Pitched] -> [Set (Note Pitched)] ---
    gen_hands' picked_notes potential_notes = ---
      case potential_notes of ---
        [] -> [] ---
        note : potential_notes' -> ---
          let ---
            picked_notes' = Set.insert note picked_notes in ---
            case is_acceptable playable picked_notes' of ---
              False -> gen_hands' picked_notes potential_notes' ---
              True -> picked_notes' : gen_hands' picked_notes' potential_notes' ---
  gen_keyboard_confs :: [Note Pitched] -> [Keyboard (Set (Note Pitched))] ---
  gen_keyboard_confs notes = Keyboard Set.empty (Set.fromList notes) : gen_keyboard_confs' Set.empty notes ---
  gen_keyboard_confs' :: Set (Note Pitched) -> [Note Pitched] -> [Keyboard (Set (Note Pitched))] ---
  gen_keyboard_confs' l r = ---
    case r of ---
      [] -> [] ---
      note : r' -> ---
        let ---
          l' = Set.insert note l in ---
          Keyboard l' (Set.fromList r') : gen_keyboard_confs' l' r' ---
  hand_span :: Set (Note Pitched) -> Int ---
  hand_span notes = ---
    case Set.elems notes of ---
      [] -> -1 ---
      _ -> distance_in_semitones (minimum notes) (maximum notes) ---
  head_and_tail :: Length_fraction -> [Event' (Notes_in')] -> (Notes_in, [Event' (Notes_in')]) ---
  head_and_tail target_len events = ---
    case events of ---
      [] -> undefined ---
      Event' notes len : events' -> ---
        ( ---
          transf_notes_in notes, ---
          case () of ---
            () | target_len == len -> events' ---
            () -> Event' (notes_to_tie notes) (len - target_len) : events') ---
  is_acceptable :: [Playable] -> Set (Note Pitched) -> Bool ---
  is_acceptable playable notes = ---
    case Set.elems notes of ---
      [] -> True ---
      note@(Pitched_note _ note_name) : notes' -> ---
        any ---
          (\ (Playable intervs starts) -> intervs == tointervs note notes' && any ((==) (semitones_from_c note_name)) starts) ---
          playable ---
  -- | Check if this configuration is playable on the keyboard. ---
  is_playable :: [Playable] -> Keyboard (Set (Note Pitched)) -> Bool ---
  is_playable playable k@(Keyboard l r) = ---
    is_acceptable (mirror_playable <$> playable) l && is_acceptable playable r && not_crossing k ---
  join_notes :: Notes_in -> Notes_in -> Notes_in ---
  join_notes (Notes_in notes_0 ties_0) (Notes_in notes_1 ties_1) = ---
    Notes_in (Set.union notes_0 notes_1) (Set.union ties_0 ties_1) ---
  join_notes_out :: [Event' Notes_out] -> [Event_fraction Pitched] ---
  join_notes_out events = ---
    case events of ---
      [] -> [] ---
      Event' notes len : events' -> join_notes_out' (Event' (notes_to_set notes) len) events' ---
  join_notes_out' :: Event_fraction Pitched -> [Event' Notes_out] -> [Event_fraction Pitched] ---
  join_notes_out' (Event' notes_0 len_0) events = ---
    case events of ---
      [] -> [Event' notes_0 len_0] ---
      Event' maybe_notes_1 len_1 : events' -> ---
        case maybe_notes_1 of ---
          Rest_out -> ---
            case Set.elems notes_0 of ---
              [] -> join_notes_out' (Event' Set.empty (len_0 + len_1)) events' ---
              _ : _ -> Event' notes_0 len_0 : join_notes_out' (Event' Set.empty len_1) events' ---
          Notes_out notes_1 -> Event' notes_0 len_0 : join_notes_out' (Event' notes_1 len_1) events' ---
          Tie_out -> join_notes_out' (Event' notes_0 (len_0 + len_1)) events' ---
  keyboard :: [Playable] -> Maybe String -> MIDI_instrument -> Score -> Either Error Score ---
  keyboard playable header_instrument midi_instrument (Score {title = score_title, header, parts}) = ---
    do ---
      keyboard_parts <- traverse keyboard_part parts ---
      Right (Score {title = score_title, header = keyboard_header, parts = keyboard_parts}) where ---
    gen_variants :: Set (Note Pitched) -> [Keyboard (Set (Note Pitched))] ---
    gen_variants notes = ---
      List.filter ---
        not_crossing ---
        ( ---
          Keyboard <$> ---
          gen_hands (mirror_playable <$> playable) (Set.elems notes) <*> ---
          gen_hands playable (reverse (Set.elems notes))) ---
    keyb :: Keyboard (Set (Note Pitched)) -> [Event' Notes_in] -> Keyboard [Event' Notes_out] ---
    keyb (Keyboard old_notes_lft old_notes_rght) events = ---
      case events of ---
        [] -> pure [] ---
        Event' (Notes_in notes ties) len : events' -> ---
          let ---
            Keyboard lft rght = ---
              head ---
                (( ---
                  filter_by_key maximum right_hand_note_count <$> ---
                  filter_by_key minimum narrower_hand_span <$> ---
                  filter_by_key minimum wider_hand_note_count <$> ---
                  filter_by_key minimum wider_hand_span <$> ---
                  filter_by_key minimum crossing_middle_c <$> ---
                  filter_by_key maximum number_of_notes) ---
                  (gen_variants notes)) ---
            (cont_notes_arg_lft, cont_notes_res_lft) = ---
              case Set.elems lft of ---
                [] -> ---
                  case Set.elems (Set.union old_notes_lft ties) of ---
                    [] -> (Set.empty, Rest_out) ---
                    _ : _ -> (old_notes_lft, Tie_out) ---
                _ : _ -> (lft, Notes_out lft) ---
            (cont_notes_arg_rght, cont_notes_res_rght) = ---
              case Set.elems rght of ---
                [] -> ---
                  case Set.elems (Set.union old_notes_rght ties) of ---
                    [] -> (Set.empty, Rest_out) ---
                    _ : _ -> (old_notes_rght, Tie_out) ---
                _ : _ -> (rght, Notes_out rght) in ---
            ( ---
              (:) <$> ---
              ((\ n -> Event' n len) <$> Keyboard cont_notes_res_lft cont_notes_res_rght) <*> ---
              keyb (Keyboard cont_notes_arg_lft cont_notes_arg_rght) events') ---
    keyboard_header :: Map Header_field String ---
    keyboard_header = ---
      case header_instrument of ---
        Nothing -> header ---
        Just header_instrument' -> Map.insert Instrument header_instrument' header ---
    keyboard_part :: Part -> Either Error Part ---
    keyboard_part (Part {title = part_title, key, time, initial_position, tempo, stave_groups}) = ---
      do ---
        let tracks = remove_notational_information stave_groups ---
        _ <- tracks_length tracks ---
        Keyboard left_hand_track right_hand_track <- ---
          traverse ---
            (notate time initial_position) ---
            ( ---
              join_notes_out <$> ---
              (keyb ---
                (pure Set.empty) ---
                (events_to_events_in ---
                  (Maybe.mapMaybe ---
                    (pitched_or_unpitched (\ (Track {events}) -> Just events) (\ _ -> Nothing)) ---
                    (remove_notational_information stave_groups))))) ---
        Right ---
          (Part { ---
            title = part_title, ---
            key, ---
            time, ---
            initial_position, ---
            tempo, ---
            stave_groups = ---
              [ ---
                [ ---
                  Pitched ---
                    (Instrument_clefs_and_staves { ---
                      instrument_name = "", ---
                      midi_instrument, ---
                      velocity = max_velocity, ---
                      clefs_and_staves = ---
                        [ ---
                          Clef_and_stave { ---
                            clef = Pitched_clef {clef_name = Treble, transposition = 0}, ---
                            stave = One_track right_hand_track}, ---
                          Clef_and_stave { ---
                            clef = Pitched_clef {clef_name = Bass, transposition = 0}, ---
                            stave = One_track left_hand_track}]})]]}) ---
  mirror_key :: Int -> Int ---
  mirror_key key = mod (4 - key) 12 ---
  mirror_playable :: Playable -> Playable ---
  mirror_playable (Playable intervals keys) = Playable (reverse intervals) (Set.fromList (mirror_key <$> Set.elems keys)) ---
  nat_char :: Char_class -> Maybe Char ---
  nat_char char_class = ---
    case char_class of ---
      Zero_char -> Just '0' ---
      Nonzero_nat_char c -> Just c ---
      _ -> Nothing ---
  narrower_hand_span :: Keyboard (Set (Note Pitched)) -> Int ---
  narrower_hand_span k = minimum (hand_span <$> k) ---
  next_location :: Char_class -> Location -> Location ---
  next_location char_class = ---
    case char_class of ---
      Delimiter_char Newline_token -> next_line ---
      _ -> next_char ---
  nonzero_nat_char :: Char_class -> Maybe Char ---
  nonzero_nat_char char_class = ---
    case char_class of ---
      Nonzero_nat_char c -> Just c ---
      _ -> Nothing ---
  not_crossing :: Keyboard (Set (Note Pitched)) -> Bool ---
  not_crossing (Keyboard l r) = ---
    case (Set.elems l, Set.elems r) of ---
      (_ : _, _ : _) -> Set.findMax l < Set.findMin r ---
      _ -> True ---
  notes_to_set :: Notes_out -> Set (Note Pitched) ---
  notes_to_set maybe_notes = ---
    case maybe_notes of ---
      Rest_out -> Set.empty ---
      Notes_out notes -> notes ---
      Tie_out -> Set.empty ---
  notes_to_tie :: Notes_in' -> Notes_in' ---
  notes_to_tie maybe_notes = ---
    case maybe_notes of ---
      Notes_in' notes -> Tie_in' notes ---
      Tie_in' notes -> Tie_in' notes ---
  number_of_notes :: Keyboard (Set (Note Pitched)) -> Int ---
  number_of_notes k = sum (Set.size <$> k) ---
  parse_key :: Parser Int ---
  parse_key = filter_parser ((>=) 11) (Is_out_of_range_with_location Key_IOOR) parse_nat ---
  parse_keys :: Parser (Set Int) ---
  parse_keys = fmap_filter_parser construct_keys (parse_list' parse_key) ---
  parse_list' :: Parser t -> Parser [t] ---
  parse_list' parse_t = parse_square_brackets (parse_many parse_t) ---
  parse_nat :: Parser Int ---
  parse_nat = parse_zero <+> parse_positive_int ---
  parse_playable :: File_path -> ExceptT Error IO [Playable] ---
  parse_playable file_path = ---
    do ---
      file <- read_file "key" readFile File_error file_path ---
      except (fromJust (parse' classify_char next_location tokenise parse_playable' Parse_error file)) ---
  parse_playable' :: Parser [Playable] ---
  parse_playable' = parse_list Newline_token (Playable <$> parse_list' parse_positive_int <*> parse_keys) ---
  parse_positive_int :: Parser Int ---
  parse_positive_int = parse_token' positive_int_token ---
  parse_square_brackets :: Parser t -> Parser t ---
  parse_square_brackets = parse_brackets Left_square_bracket_token Right_square_bracket_token ---
  parse_zero :: Parser Int ---
  parse_zero = ---
    do ---
      parse_token Zero_token ---
      return 0 ---
  positive_int_token :: Token -> Maybe Int ---
  positive_int_token token = ---
    case token of ---
      Positive_int_token i -> Just i ---
      _ -> Nothing ---
  right_hand_note_count :: Keyboard (Set (Note Pitched)) -> Int ---
  right_hand_note_count (Keyboard _ r) = Set.size r ---
  -- | Check if the set of notes is playable on the keyboard. ---
  set_is_playable :: [Playable] -> Set (Note Pitched) -> Bool ---
  set_is_playable playable notes = any (is_playable playable) (gen_keyboard_confs (Set.elems notes)) ---
  tointervs :: Note Pitched -> [Note Pitched] -> [Int] ---
  tointervs note_0 notes = ---
    case notes of ---
      [] -> [] ---
      note_1 : notes' -> distance_in_semitones note_0 note_1 : tointervs note_1 notes' ---
  tokenise :: Tokeniser () ---
  tokenise = void (parse_many tokenise_1) ---
  tokenise_1 :: Tokeniser () ---
  tokenise_1 = tokenise_delimiter <+> tokenise_nat <+> tokenise_whitespace ---
  tokenise_delimiter :: Tokeniser () ---
  tokenise_delimiter = add_token (parse_token' delimiter_char) ---
  tokenise_nat :: Tokeniser () ---
  tokenise_nat = add_token (tokenise_zero <+> tokenise_positive_int) ---
  tokenise_positive_int :: Tokeniser Token ---
  tokenise_positive_int = ---
    Positive_int_token <$> read <$> ((:) <$> parse_token' nonzero_nat_char <*> parse_many (parse_token' nat_char)) ---
  tokenise_whitespace :: Tokeniser () ---
  tokenise_whitespace = parse_token Whitespace_char ---
  tokenise_zero :: Tokeniser Token ---
  tokenise_zero = ---
    do ---
      parse_token Zero_char ---
      return Zero_token ---
  transf_notes_in :: Notes_in' -> Notes_in ---
  transf_notes_in maybe_notes = ---
    case maybe_notes of ---
      Notes_in' notes -> Notes_in notes Set.empty ---
      Tie_in' notes -> Notes_in Set.empty notes ---
  wider_hand_span :: Keyboard (Set (Note Pitched)) -> Int ---
  wider_hand_span k = maximum (hand_span <$> k) ---
  wider_hand_note_count :: Keyboard (Set (Note Pitched)) -> Int ---
  wider_hand_note_count k = Set.size (maximumBy (comparing hand_span) k) ---
{- ---
  calc_intervs :: Note Pitched -> [Note Pitched] -> [Int] ---
  calc_intervs note notes = ---
    case notes of ---
      [] -> [] ---
      note' : notes' -> distance_in_semitones note note' : calc_intervs note' notes' ---
  cat_head_tail :: Length_fraction -> Notes_in -> [Event_out] -> [Event_out] ---
  cat_head_tail len notes_in evs = ---
    case (notes_in, evs) of ---
      (Rest_in, []) -> [Event_out Rest_out len] ---
      (Rest_in, Event_out Rest_out len' : evs') -> Event_out Rest_out (len + len') : evs' ---
      (Rest_in, Event_out (Notes_out notes') len' : evs') -> Event_out Rest_out len : Event_out (Notes_out notes') len' : evs' ---
      (Rest_in, Event_out Tie_out _ : _) -> undefined ---
      (Notes_in notes, []) -> [Event_out (Notes_out notes) len] ---
      (Notes_in notes, Event_out Rest_out len' : evs') -> Event_out (Notes_out notes) len : Event_out Rest_out len' : evs' ---
      (Notes_in notes, Event_out (Notes_out notes') len' : evs') -> ---
        Event_out (Notes_out notes) len : Event_out (Notes_out notes') len' : evs' ---
      (Notes_in notes, Event_out Tie_out len' : evs') -> Event_out (Notes_out notes) (len + len') : evs' ---
      (Tie_in _, []) -> [Event_out Tie_out len] ---
      (Tie_in _, Event_out Rest_out len' : evs') -> Event_out Tie_out len : Event_out Rest_out len' : evs' ---
      (Tie_in _, Event_out (Notes_out notes') len' : evs') -> Event_out Tie_out len : Event_out (Notes_out notes') len' : evs' ---
      (Tie_in _, Event_out Tie_out len' : evs') -> Event_out Tie_out (len + len') : evs' ---
  choose_next_chord :: [([Int], [Int])] -> Keyboard Old_notes -> Set (Note Pitched) -> Set (Note Pitched) -> Keyboard Notes_in ---
  choose_next_chord acceptable_combinations old_notes potential_tied_notes potential_new_notes = ---
      min_wider_hand_range = minimum (wider_hand_range <$> all_lrs_3) ---
      all_lrs_4 = List.filter (\ var -> min_wider_hand_range == wider_hand_range var) all_lrs_3 ---
      min_max_notes = minimum (max_notes <$> all_lrs_4) ---
      all_lrs_5 = List.filter (\ var -> min_max_notes == max_notes var) all_lrs_4 ---
      min_narrower_hand_range = minimum (narrower_hand_range <$> all_lrs_5) ---
      all_lrs_6 = List.filter (\ var -> min_narrower_hand_range == narrower_hand_range var) all_lrs_5 ---
      max_right_hand_notes = maximum (right_hand_notes <$> all_lrs_6) ---
      all_lrs_7 = List.filter (\ var -> max_right_hand_notes == right_hand_notes var) all_lrs_6 in ---
      find_res potential_tied_notes <$> old_notes <*> head all_lrs_7 ---
  constr_events_in :: [Event_fraction Pitched] -> Events_in ---
  constr_events_in events = ---
    case events of ---
      [] -> Empty_events_in ---
      Event_fraction notes len : events' -> Construct_events_in (First_event_in (constr_notes_in notes) len) events' ---
  constr_notes_in :: Set (Note Pitched) -> Notes_in ---
  constr_notes_in notes = ---
    case Set.elems notes of ---
      [] -> Rest_in ---
      _ -> Notes_in notes ---
  constr_events_out :: Time -> Initial_position -> [Event_out] -> Either Error [Event Pitched] ---
  constr_events_out time initial_position events = notate time initial_position (constr_events_out' events) ---
  constr_events_out' :: [Event_out] -> [Event_fraction Pitched] ---
  constr_events_out' events = ---
    case events of ---
      [] -> [] ---
      [Event_out notes len] -> ---
        [ ---
          Event_fraction ---
            (case notes of ---
              Rest_out -> Set.empty ---
              Notes_out notes' -> notes' ---
              Tie_out -> undefined) ---
            len] ---
      Event_out notes_0 len_0 : Event_out notes_1 len_1 : events' -> ---
        case (notes_0, notes_1) of ---
          (Rest_out, Rest_out) -> constr_events_out' (Event_out Rest_out (len_0 + len_1) : events') ---
          (Rest_out, Notes_out _) -> ---
            Event_fraction Set.empty len_0 : constr_events_out' (Event_out notes_1 len_1 : events') ---
          (Notes_out notes'_0, Rest_out) -> ---
            Event_fraction notes'_0 len_0 : constr_events_out' (Event_out notes_1 len_1 : events') ---
          (Notes_out notes'_0, Notes_out _) -> ---
            Event_fraction notes'_0 len_0 : constr_events_out' (Event_out notes_1 len_1 : events') ---
          (Notes_out _, Tie_out) -> constr_events_out' (Event_out notes_0 (len_0 + len_1) : events') ---
          _ -> undefined ---
-} ---
{- ---
  filter_new_notes :: Notes_in -> Maybe (Set (Note Pitched)) ---
  filter_new_notes notes_in = ---
    case notes_in of ---
      New_notes_in notes -> Just notes ---
      _ -> Nothing ---
  filter_old_notes :: Notes_in -> Maybe (Set (Note Pitched)) ---
  filter_old_notes notes_in = ---
    case notes_in of ---
      Old_notes_in notes -> Just notes ---
      _ -> Nothing ---
  filters :: Keyboard (Set (Note Pitched)) -> [Keyboard (Set (Note Pitched))] -> Keyboard (Set (Note Pitched)) ---
  filters current_notes = _ ---
-} ---
{- ---
  find_res :: Set (Note Pitched) -> Old_notes -> Set (Note Pitched) -> Notes_in ---
  find_res potential_tied_notes old_notes new_notes = ---
    case Set.null new_notes of ---
      False -> Notes_in new_notes ---
      True -> ---
        case old_notes of ---
          Old_notes_rest -> Rest_in ---
          Old_notes_notes old_notes' -> ---
            case Set.null (Set.intersection potential_tied_notes old_notes') of ---
              False -> Tie_in old_notes' ---
              True -> Rest_in ---
  is_acceptable :: Playable -> [Note Pitched] -> Bool ---
  is_acceptable acceptable_combinations notes = ---
    case notes of ---
      [] -> True ---
      note : notes' -> any (is_acceptable' note notes') acceptable_combinations ---
  is_acceptable' :: Note Pitched -> [Note Pitched] -> ([Int], [Int]) -> Bool ---
  is_acceptable' note@(Pitched_note _ note_name) notes (intervs, strts) = ---
    calc_intervs note notes == intervs && elem (semitones_from_c note_name) strts ---
  keyb :: [Playable] -> Keyboard Old_notes -> [[Event' Notes_in]] -> Keyboard [Event' Notes_out] ---
  keyb playable old_notes tracks = ---
    case all ((==) Empty_events_in) tracks of ---
      False -> ---
        let ---
          (len, cont_notes, new_notes, tracks') = head_and_tail tracks ---
          next_notes = choose_next_chord acceptable_combinations old_notes cont_notes new_notes ---
          next_events = keyb acceptable_combinations (transf_notes' <$> next_notes) tracks' in ---
          cat_head_tail len <$> next_notes <*> next_events ---
      True -> pure [] ---
  keyboard :: File_path -> String -> Instrument Pitched -> File_path -> Directory -> String -> IO () ---
  keyboard acceptable_combinations_file_path instrument_name midi_instrument score_file_path dir fname = ---
    do ---
      res <- runExceptT (keyboard_h acceptable_combinations_file_path instrument_name midi_instrument score_file_path) ---
      case res of ---
        Left err -> putStrLn (write_error err) ---
        Right score -> write dir fname score ---
  keyboard_h :: File_path -> String -> Instrument Pitched -> File_path -> ExceptT Error IO Score ---
  keyboard_h acceptable_combinations_file_path instrument_name midi_instrument score_file_path = ---
    do ---
      acceptable_combinations <- parse_keyboard acceptable_combinations_file_path ---
      score <- parse score_file_path ---
      ExceptT (return (keyboard_help acceptable_combinations instrument_name midi_instrument score)) ---
-} ---
{- ---
        let keyb_res = ---
              keyb ---
                acceptable_combinations ---
                (pure Old_notes_rest) ---
                (catMaybes ---
                  (pitched_or_unpitched (\ (Track {events}) -> Just (constr_events_in events)) (return Nothing) <$> tracks)) ---
        Keyboard left_hand_track right_hand_track <- traverse (constr_events_out time initial_position) keyb_res ---
  max_notes :: Keyboard (Set (Note Pitched)) -> Int ---
  max_notes k = maximum (Set.size <$> k) ---
  narrower_hand_range :: Keyboard (Set (Note Pitched)) -> Int ---
  narrower_hand_range k = minimum (hand_range <$> k) ---
-} ---
{- ---
  notes_out_to_notes_0 :: Notes_out -> Set (Note Pitched) ---
  notes_out_to_notes_0 notes_out = ---
    case notes_out of ---
      New_notes_out notes -> notes ---
      Old_notes_out notes -> Tie ---
      Rest_notes_out -> Set.empty ---
-} ---
{- ---
  notes_out_to_notes_1 :: Notes' -> Notes Pitched ---
  notes_out_to_notes_1 notes_out = ---
    case notes_out of ---
      Notes'_rest -> Notes (Set.empty) ---
      Notes'_notes notes -> Notes notes ---
      Notes'_tie _ -> Tie ---
-} ---
{- ---
  notes_to_notes_in :: Set (Note Pitched) -> Notes Pitched -> (Notes_in, Set (Note Pitched)) ---
  notes_to_notes_in prev_notes maybe_notes = ---
    case maybe_notes of ---
      Notes notes -> ---
        ( ---
          case Set.size notes of ---
            0 -> Rest_notes_in ---
            _ -> New_notes_in notes, ---
          notes) ---
      Tie -> (Old_notes_in prev_notes, prev_notes) ---
-} ---
{- ---
  only_new :: Notes_in -> Set (Note Pitched) ---
  only_new notes_in = ---
    case notes_in of ---
      Notes_in notes -> notes ---
      _ -> Set.empty ---
  only_ties :: Notes_in -> Set (Note Pitched) ---
  only_ties notes_in = ---
    case notes_in of ---
      Tie_in notes -> notes ---
      _ -> Set.empty ---
  right_hand_notes :: Keyboard (Set (Note Pitched)) -> Int ---
  right_hand_notes (Keyboard _ r) = Set.size r ---
-} ---
{- ---
  select_new_notes :: Set (Note Pitched) -> Set (Note Pitched) -> Set (Note Pitched) -> Notes_out ---
  select_new_notes old_notes current_notes new_notes = ---
    case () of ---
      () | Set.null new_notes -> ---
        case () of ---
          () | not (Set.null current_notes) && isSubsetOf current_notes old_notes -> Old_notes_out ---
          () -> Rest_notes_out ---
      _ -> New_notes_out new_notes ---
  seq2 :: Set (Note Pitched) -> [Event Pitched] -> Either String [Simultaneous_in] ---
  seq2 prev_notes evs = ---
    case evs of ---
      [] -> Right [] ---
      ev : evs' -> ---
        do ---
          (ev2, nxt_notes) <- simultaneous_to_simultaneous_in prev_notes ev ---
          evs2 <- seq2 nxt_notes evs' ---
          Right (ev2 : evs2) ---
-} ---
{- ---
  sequential_in_tail_to_sequential_in :: [Simultaneous_in] -> Sequential_in ---
  sequential_in_tail_to_sequential_in sequential_in_tail = ---
    case sequential_in_tail of ---
      [] -> Empty_sequential_in ---
      simultaneous_in : sequential_in_tail' -> Construct_sequential_in simultaneous_in sequential_in_tail' ---
-} ---
{- ---
  sequential_out_to_sequential :: Sequential_out -> [Event Pitched] ---
  sequential_out_to_sequential sequential_out = ---
    case sequential_out of ---
      Empty_sequential_out -> [] ---
      Construct_sequential_out simultaneous_out sequential -> simultaneous_out_to_simultaneous simultaneous_out : sequential ---
  sequential_to_sequential_in :: Old_notes -> [Event Pitched] -> Either String ([Event'], Old_notes) ---
  sequential_to_sequential_in maybe_old_notes sequential = ---
    case sequential of ---
      [] -> Right ([], maybe_old_notes) ---
      ev : sequential' -> ---
        case ev of ---
          Event maybe_new_notes len -> ---
            case maybe_new_notes of ---
              Notes new_notes -> ---
                case Set.size new_notes of ---
                  0 -> first ((:) (Event' Notes'_rest len)) <$> sequential_to_sequential_in Old_notes_rest sequential' ---
                  _ -> ---
                    ( ---
                      first ((:) (Event' (Notes'_notes new_notes) len)) <$> ---
                      sequential_to_sequential_in (Old_notes_notes new_notes) sequential') ---
              Tie -> ---
                case maybe_old_notes of ---
                  Old_notes_rest -> ---
                    first ((:) (Event' Notes'_rest len)) <$> sequential_to_sequential_in Old_notes_rest sequential' ---
                  Old_notes_notes old_notes -> ---
                    ( ---
                      first ((:) (Event' (Notes'_tie old_notes) len)) <$> ---
                      sequential_to_sequential_in (Old_notes_notes old_notes) sequential') ---
          Tuplet rat evs -> ---
            do ---
              (evs', maybe_old_notes') <- sequential_to_sequential_in maybe_old_notes evs ---
              (evs'', maybe_old_notes'') <- sequential_to_sequential_in maybe_old_notes' sequential' ---
              Right (Tuplet' rat evs' : evs'', maybe_old_notes'') ---
  simultaneous_out_to_simultaneous :: Event' -> Event Pitched ---
  simultaneous_out_to_simultaneous ev = ---
    case ev of ---
      Event' notes_out len -> Event (notes_out_to_notes_1 notes_out) len ---
      Tuplet' rat evs -> Tuplet rat (simultaneous_out_to_simultaneous <$> evs) ---
-} ---
{- ---
  simultaneous_to_simultaneous_in :: Set (Note Pitched) -> Event Pitched -> Either String (Simultaneous_in, Set (Note Pitched)) ---
  simultaneous_to_simultaneous_in prev_notes (Event notes len) = ---
    do ---
      check "Non-positive note length." (0 < len) ---
      let (notes', notes'') = notes_to_notes_in prev_notes notes ---
      Right (Simultaneous_in notes' len, notes'') ---
  tail_notes :: Notes_in -> Notes_in ---
  tail_notes notes_in = ---
    case notes_in of ---
      New_notes_in notes -> Old_notes_in notes ---
      Old_notes_in notes -> Old_notes_in notes ---
      Rest_notes_in -> Rest_notes_in ---
-} ---
{- ---
  transf_notes' :: Notes_in -> Old_notes ---
  transf_notes' notes_in = ---
    case notes_in of ---
      Rest_in -> Old_notes_rest ---
      Notes_in notes -> Old_notes_notes notes ---
      Tie_in notes -> Old_notes_notes notes ---
  transf_old_pos :: Old_notes -> Set (Note Pitched) ---
  transf_old_pos notes' = ---
    case notes' of ---
      Old_notes_rest -> Set.empty ---
      Old_notes_notes notes -> notes ---
  var_size :: Keyboard (Set (Note Pitched)) -> Int ---
  var_size k = sum (Set.size <$> k) ---
  compute_distance :: Keyboard (Set Note) -> Keyboard (Set Note) -> Semitones ---
  compute_distance keyboard_notes_0 keyboard_notes_1 = sum (compute_distance' <$> keyboard_notes_0 <*> keyboard_notes_1) ---
  compute_distance' :: Set Note -> Set Note -> Semitones ---
  compute_distance' notes_0 notes_1 = sum (abs <$> (distance_in_semitones <$> elems notes_0 <*> elems notes_1)) ---
  compute_distances_in_semitones :: [Note] -> [Semitones] ---
  compute_distances_in_semitones notes = zipWith distance_in_semitones (init notes) (tail notes) ---
  -- | The lowest and highest voices are always included. The algorithm attempts to include as many middle voices as possible ---
  -- while keeping the score playable. ---
  number_of_notes :: Keyboard (Set Note) -> Int ---
  number_of_notes keyboard_notes = sum (fmap size keyboard_notes) ---
  order :: Ord t => t -> t -> (t, t) ---
  order x y = ---
    case compare' x y of ---
      LT' -> (x, y) ---
      EQ' z -> (z, z) ---
      GT' -> (y, x) ---
  -- | Checks whether a set of notes is playable on the keyboard. ---
  playable :: Set Note -> Bool ---
  playable notes = any playable' (divide_between_hands (elems notes)) ---
  playable' :: Keyboard (Set Note) -> Bool ---
  playable' (Keyboard left_hand_notes right_hand_notes) = ---
    playable_with_the_left_hand left_hand_notes && playable_with_the_right_hand right_hand_notes ---
  playable_with_one_hand :: ---
    (Set Note -> Maybe Note) -> (Semitones -> Semitones) -> ([Semitones] -> [Semitones]) -> Set Note -> Bool ---
  playable_with_one_hand reference_note mirror_semitones_from_c mirror_distances_in_semitones notes = ---
    case reference_note notes of ---
      Nothing -> True ---
      Just (Note _ note_name) -> ---
        elem ---
          (mirror_semitones_from_c (semitones_from_c note_name)) ---
          (playable_with_the_right_hand_table (mirror_distances_in_semitones (compute_distances_in_semitones (elems notes)))) ---
  playable_with_the_left_hand :: Set Note -> Bool ---
  playable_with_the_left_hand = playable_with_one_hand lookupMax (\) reverse ---
  playable_with_the_right_hand :: Set Note -> Bool ---
  playable_with_the_right_hand = playable_with_one_hand lookupMin id id ---
  range :: Keyboard (Set Note) -> Range ---
  range (Keyboard left_hand_notes right_hand_notes) = ---
    uncurry Range (swap (order (range' left_hand_notes) (range' right_hand_notes))) ---
  range' :: Set Note -> Maybe Semitones ---
  range' notes = distance_in_semitones <$> lookupMin notes <*> lookupMax notes ---
  shape :: Keyboard (Set Note) -> Shape ---
  shape (Keyboard left_hand_notes right_hand_notes) = ---
    construct_shape (shape' id lookupMin reverse left_hand_notes) (shape' flip lookupMax id right_hand_notes) ---
  shape' :: ---
    ( ---
      ((Note -> Note -> Semitones) -> Note -> Note -> Semitones) -> ---
      (Set Note -> Maybe Note) -> ---
      ([Note] -> [Note]) -> ---
      Set Note -> ---
      Maybe [Semitones]) ---
  shape' flip_or_not get_reference_note reverse_or_not notes = ---
    do ---
      reference_note <- get_reference_note notes ---
      Just (flip_or_not distance_in_semitones reference_note <$> reverse_or_not (elems (delete reference_note notes))) ---
  wrong_order :: Maybe Note -> Maybe Note -> Maybe Semitones -> Keyboard (Set Note) -> Maybe Semitones ---
  wrong_order left_hand_limit right_hand_limit if_distance_zero (Keyboard left_hand_notes right_hand_notes) = ---
    let ---
      f = wrong_order' if_distance_zero ---
    in ---
      max (f (lookupMax left_hand_notes) left_hand_limit) (f right_hand_limit (lookupMin right_hand_notes)) ---
  wrong_order' :: Maybe Semitones -> Maybe Note -> Maybe Note -> Maybe Semitones ---
  wrong_order' if_distance_zero maybe_note_0 maybe_note_1 = ---
    do ---
      note_0 <- maybe_note_0 ---
      note_1 <- maybe_note_1 ---
      let distance = distance_in_semitones note_0 note_1 ---
      case compare distance 0 of ---
        LT -> Just (negate distance) ---
        EQ -> if_distance_zero ---
        GT -> Nothing ---
  wrong_stave :: Keyboard (Set Note) -> Maybe Semitones ---
  wrong_stave = wrong_order (Just (Note 4 C)) (Just (Note 4 C)) Nothing ---
-} ---