{-|
Generates Lilypond files.
-}
module Composition.Lilypond (lilypond) where
  import Composition.Errors
  import Composition.Notation
  import Composition.Notes
  import Composition.Score
  import Composition.Theory
  import Control.Lens.Combinators hiding (index, parts)
  import Control.Lens.Operators
  import Control.Monad.Except
  import Control.Monad.State.Strict hiding (State)
  import Control.Monad.Trans.Except
  import Data.Bifunctor
  import Data.Char
  import Data.Foldable
  import Data.Generics.Labels ()
  import Data.List
  import Data.Map.Strict as Map
  import Data.Set as Set
  import GHC.Generics
  import GHC.Real
  import Parser.Files
  import Parser.Utilities
  data Header_field' = Header_field' String String
  data State = State {state_notes :: Bool, state_position :: Position}
  deriving instance Generic State
  deriving instance Show Header_field'
  deriving instance Show State
  check_char :: Char -> Either Error ()
  check_char c = check Invalid_character_in_text (elem c " !#$%&'()*+,-./:;<=>?@[]^_`{|}~" || isDigit c || isLetter c)
  convert_header_field :: (Header_field, String) -> Header_field'
  convert_header_field (name, value) = Header_field' write_header_field_name value where
    write_header_field_name :: String
    write_header_field_name =
      case name of
        Left_text -> "poet"
        Overtitle -> "dedication"
        Right_text -> "composer"
        Subtitle -> "subtitle"
        Subsubtitle -> "subsubtitle"
        Title -> "title"
  -- | Encodes the score in Lilypond format and writes it to the specified file.
  lilypond :: Score -> File_path -> ExceptT Error IO ()
  lilypond score file_path =
    do
      written_score <- except (write_score score)
      write_file "ly" writeFile File_error file_path written_score
  write_accidental :: Accidental -> String
  write_accidental accidental =
    case accidental of
      Flat -> "f"
      Natural -> ""
      Sharp -> "s"
  write_angular_brackets :: String -> String
  write_angular_brackets = write_brackets "<" ">"
  write_angular_brackets_2 :: String -> String
  write_angular_brackets_2 = write_brackets "<<" ">>"
  write_basic_length :: Basic_length -> String
  write_basic_length len = show (basic_length_denominator len)
  write_brackets :: String -> String -> String -> String
  write_brackets left_bracket right_bracket x = left_bracket <> x <> right_bracket
  write_clef_name :: Clef_name -> String
  write_clef_name clef_name =
    case clef_name of
      Subbass -> "subbass"
      Bass -> "bass"
      Baritone_F -> "baritonevarF"
      Baritone_C -> "baritone"
      Tenor -> "tenor"
      Alto -> "alto"
      Mezzosoprano -> "mezzosoprano"
      Soprano -> "soprano"
      Treble -> "treble"
      French -> "french"
  write_curly_brackets :: String -> String
  write_curly_brackets = write_brackets "{" "}"
  write_dot :: Dot -> Char
  write_dot Dot = '.'
  write_eq :: String -> String -> String
  write_eq x y = x <-> "=" <-> y
  write_header :: [Header_field'] -> Either Error String
  write_header header =
    do
      fields <- traverse write_header_field header
      Right ("\\header" <-> write_curly_brackets (intercalate " " fields))
  write_header_field :: Header_field' -> Either Error String
  write_header_field (Header_field' name value) = write_eq name <$> write_quotes value
  write_key_accidental :: Accidental -> String
  write_key_accidental accidental = toUpper <$> show accidental
  write_key_note_name :: Map Natural_note_name [Accidental] -> Natural_note_name -> Either Error String
  write_key_note_name key natural_note_name =
    (case Map.lookup natural_note_name key of
      Nothing -> write_key_note_name' Natural
      Just accidentals' ->
        case accidentals' of
          [accidental] -> write_key_note_name' accidental
          _ -> Left Conflicting_accidentals_in_key_signature) where
    write_key_natural_note_name :: String
    write_key_natural_note_name = show (fromEnum natural_note_name)
    write_key_note_name' :: Accidental -> Either Error String
    write_key_note_name' accidental =
      Right (write_round_brackets (write_key_natural_note_name <-> ". ," <> write_key_accidental accidental))
  write_language :: Either Error String
  write_language =
    do
      language <- write_quotes "english.ly"
      Right ("\\include" <-> language)
  write_length :: Length -> String
  write_length (Length len dots) = write_basic_length len <> (write_dot <$> dots)
  write_natural_note_name :: Natural_note_name -> String
  write_natural_note_name natural_note_name = [toLower (head (show natural_note_name))]
  write_note :: Note Pitched -> String
  write_note (Pitched_note octave note_name) = write_note_name note_name <> write_octave octave
  write_note_name :: Note_name -> String
  write_note_name note_name =
    let
      (natural_note_name, accidental) = deconstruct_note_name note_name in
      write_natural_note_name natural_note_name <> write_accidental accidental
  write_notes :: Notes Pitched -> StateT State (Either Error) (Bool, String)
  write_notes maybe_notes =
    case maybe_notes of
      Notes notes ->
        do
          #state_notes .= not (Set.null notes)
          return
            (
              False,
              case Set.elems notes of
                [] -> "r"
                notes' -> write_angular_brackets (intercalate " " (write_note <$> notes')))
      Tie ->
        do
          notes <- use #state_notes
          return
            (
              notes,
              case notes of
                False -> "r"
                True -> "")
  write_octave :: Octave -> String
  write_octave octave =
    case compare octave 3 of
      LT -> replicate (3 - octave) ','
      EQ -> ""
      GT -> replicate (octave - 3) '\''
  write_option :: String -> String -> String
  write_option name value = "#" <> write_round_brackets ("ly:set-option '" <> name <-> value)
  write_options :: String
  write_options = write_option "delete-intermediate-files" "#t" <-> write_option "no-point-and-click" "#t"
  write_override :: String -> String -> String
  write_override name value = "\\override" <-> write_eq name value
  write_part :: Part -> Either Error String
  write_part (Part {title, key, time, initial_position, stave_groups}) =
    do
      header <- write_header [Header_field' "piece" title]
      set_key <- write_set_key
      time_and_initial_position <- write_time_and_initial_position
      _ <- tracks_length (remove_notational_information stave_groups)
      written_stave_groups <- traverse write_stave_group stave_groups
      Right
        (
          set_key <->
          time_and_initial_position <->
          "\\score" <->
          write_curly_brackets (header <-> write_angular_brackets_2 (intercalate " " written_stave_groups))) where
    write_event :: Int -> Event Pitched -> StateT State (Either Error) (Bool, String)
    write_event triplets event =
      case event of
        Event notes len ->
          do
            State {state_position} <- get
            let position = state_position + (2 % 3) ^ triplets * length_to_fraction len
            position' <-
              case compare position (measure_length time) of
                LT -> return position
                EQ -> return 0
                GT -> throwError An_event_crosses_the_bar_line
            #state_position .= position'
            (tie, written_notes) <- write_notes notes
            return (tie, written_notes <> write_length len)
        Triplet events ->
          do
            maybe_events <- write_events' (1 + triplets) events
            case maybe_events of
              Nothing -> throwError Empty_triplet
              Just (tie, written_events) -> return (tie, "\\tuplet 3/2" <-> write_curly_brackets written_events)
    write_events :: [Event Pitched] -> Either Error String
    write_events events =
      do
        initial_position' <- initial_position_to_fraction time initial_position
        maybe_events <- evalStateT (write_events' 0 events) (State {state_notes = False, state_position = initial_position'})
        Right
          (case maybe_events of
            Nothing -> ""
            Just (_, written_events) -> written_events)
    write_events' :: Int -> [Event Pitched] -> StateT State (Either Error) (Maybe (Bool, String))
    write_events' triplets events =
      case events of
        [] -> return Nothing
        event : events' ->
          do
            (tie_0, written_event) <- write_event triplets event
            maybe_events <- write_events' triplets events'
            return
              (Just
                (
                  tie_0,
                  intercalate
                    " "
                    (
                      written_event :
                      case maybe_events of
                        Nothing -> []
                        Just (tie_1, written_events) ->
                          (
                            (case tie_1 of
                              False -> []
                              True -> ["~"]) <>
                            [written_events]))))
    write_initial_position :: Either Error (Maybe String)
    write_initial_position =
      do
        initial_position' <- initial_position_to_fraction time initial_position
        let num :% den = measure_length time - initial_position'
        Right
          (case initial_position' of
            0 -> Nothing
            _ -> Just (show den <-> "*" <-> show num))
    write_instrument_and_staves :: Instrument_and_staves note_type -> Either Error String
    write_instrument_and_staves (Instrument_and_staves {instrument_name, short_instrument_name, staves}) =
      do
        set_instrument_name <- write_set_instrument_name
        written_staves <- traverse write_stave staves
        Right
          (
            "\\new PianoStaff \\with" <->
            write_curly_brackets set_instrument_name <->
            write_angular_brackets_2 (intercalate " " written_staves)) where
      write_set_instrument_name :: Either Error String
      write_set_instrument_name =
        do
          written_instrument_name <- write_quotes instrument_name
          written_short_instrument_name <- write_quotes short_instrument_name
          Right
            (write_eq "instrumentName" written_instrument_name <-> write_eq "shortInstrumentName" written_short_instrument_name)
    write_key :: Either Error String
    write_key =
      do
        written_key <-
          traverse
            (write_key_note_name (fromListWith (<>) (second return <$> deconstruct_note_name <$> Set.elems key)))
            (enumFromTo C_natural B_natural)
        Right (write_note_name C <-> "#`" <> write_round_brackets (intercalate " " written_key))
    write_set_initial_position :: Either Error [String]
    write_set_initial_position =
      do
        maybe_written_initial_position <- write_initial_position
        Right
          (case maybe_written_initial_position of
            Nothing -> []
            Just written_initial_position -> ["\\partial" <-> written_initial_position])
    write_set_key :: Either Error String
    write_set_key =
      do
        written_key <- write_key
        Right (write_eq "Key" (write_curly_brackets ("\\key" <-> written_key)))
    write_set_time :: String
    write_set_time = "\\time" <-> write_time time
    write_stave :: Stave note_type -> Either Error String
    write_stave (Stave {clef, tracks}) =
      do
        set_clef <- write_set_clef
        written_tracks <- write_tracks
        Right
          (
            "\\new Staff \\with" <->
            write_curly_brackets
              (case clef of
                Percussion_clef ->
                  (
                    write_override "StaffSymbol.line-count" (show (length written_tracks)) <->
                    write_override "Stem.neutral-direction" "1")
                Pitched_clef {} -> "") <->
            write_curly_brackets
              (
                intercalate
                  " "
                  (
                    (case clef of
                      Percussion_clef -> []
                      Pitched_clef {} -> ["\\Key"]) <>
                    ["\\Time_and_initial_position", set_clef]) <->
                write_angular_brackets_2 written_tracks <->
                "\\bar \"|.\"")) where
      write_clef :: String
      write_clef =
        case clef of
          Percussion_clef -> "percussion"
          Pitched_clef {clef_name, transposition} -> write_clef_name clef_name <> write_transposition transposition
      write_set_clef :: Either Error String
      write_set_clef =
        do
          written_clef <- write_quotes write_clef
          Right ("\\clef" <-> written_clef)
      write_tracks :: Either Error String
      write_tracks =
        case tracks of
          One_track events -> write_track "\\oneVoice" (Pitched_note 4 C) events
          Two_tracks events_0 events_1 ->
            (
              (<->) <$>
              write_track "\\voiceOne" (Pitched_note 4 D) events_0 <*>
              write_track "\\voiceTwo" (Pitched_note 3 B) events_1)
    write_stave_group :: [Pitched_or_unpitched Instrument_and_staves] -> Either Error String
    write_stave_group stave_group =
      do
        instruments_and_staves <- traverse (pitched_and_unpitched write_instrument_and_staves) stave_group
        Right
          (case instruments_and_staves of
            [instrument_and_staves] -> instrument_and_staves
            _ -> "\\new StaffGroup" <-> write_angular_brackets_2 (intercalate " " instruments_and_staves))
    write_time_and_initial_position :: Either Error String
    write_time_and_initial_position =
      do
        set_initial_position <- write_set_initial_position
        Right (
          write_eq
            "Time_and_initial_position"
            (write_curly_brackets (intercalate " " (["\\numericTimeSignature", write_set_time] <> set_initial_position))))
    write_track :: String -> Note Pitched -> [Event note_type] -> Either Error String
    write_track voice unpitched_note events =
      do
        written_events <- write_events (convert_events_to_pitched events)
        Right ("\\new Voice" <-> write_curly_brackets (voice <-> write_curly_brackets written_events)) where
      convert_event_to_pitched :: Event note_type -> Event Pitched
      convert_event_to_pitched event =
        case event of
          Event notes len -> Event (convert_notes_to_pitched notes) len
          Triplet events' -> Triplet (convert_events_to_pitched events')
      convert_events_to_pitched :: [Event note_type] -> [Event Pitched]
      convert_events_to_pitched = (<$>) convert_event_to_pitched
      convert_note_to_pitched :: Note note_type -> Note Pitched
      convert_note_to_pitched note =
        case note of
          Pitched_note _ _ -> note
          Unpitched_note -> unpitched_note
      convert_notes_to_pitched :: Notes note_type -> Notes Pitched
      convert_notes_to_pitched maybe_notes =
        case maybe_notes of
          Notes notes -> Notes (mapMonotonic convert_note_to_pitched notes)
          Tie -> Tie
  write_quotes :: String -> Either Error String
  write_quotes text =
    do
      traverse_ check_char text
      Right (write_brackets "\"" "\"" text)
  write_round_brackets :: String -> String
  write_round_brackets = write_brackets "(" ")"
  write_score :: Score -> Either Error String
  write_score (Score {header, parts}) =
    do
      written_parts <- traverse write_part parts
      language <- write_language
      written_header <- write_header (Header_field' "tagline" "" : (convert_header_field <$> assocs header))
      Right
        (write_options <-> language <-> "\\layout {short-indent = 5\\mm}" <-> written_header <-> intercalate " " written_parts)
  write_time :: Time -> String
  write_time (Time num den) = write_time_numerator <> "/" <> write_basic_length den where
    write_time_numerator :: String
    write_time_numerator = show (time_numerator_to_int num)
  write_transposition :: Steps -> String
  write_transposition transposition =
    case compare transposition 0 of
      LT -> write_transposition' "_" (negate transposition)
      EQ -> ""
      GT -> write_transposition' "^" transposition
  write_transposition' :: String -> Steps -> String
  write_transposition' index transposition = index <> show transposition