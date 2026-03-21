{-| ---
Description: Generating Lilypond files. ---
 ---
* Generating Lilypond files ---
-} ---
module Composition.Lilypond (lilypond) where ---
  import Composition.Errors ---
  import Composition.Notes ---
  import Composition.Score ---
  import Composition.Theory ---
  import Composition.Time ---
  import Control.Lens.Combinators hiding (index, parts) ---
  import Control.Lens.Operators ---
  import Control.Monad.Except ---
  import Control.Monad.State.Strict ---
  import Control.Monad.Trans.Except ---
  import Data.Bifunctor ---
  import Data.Char ---
  import Data.Foldable ---
  import Data.Generics.Labels () ---
  import Data.List ---
  import Data.Map.Strict as Map ---
  import Data.Set as Set ---
  import GHC.Generics ---
  import GHC.Real ---
  import Parser.Files ---
  import Parser.Utilities ---
  data Header_field' = Header_field' String String ---
  data State = State {state_notes :: Bool, state_position :: Position} ---
  deriving instance Generic Composition.Lilypond.State ---
  deriving instance Show Header_field' ---
  check_char :: Char -> Either Error () ---
  check_char c = check Invalid_character_in_text (elem c " !#$%&'()*+,-./:;<=>?@[]^_`{|}~" || isDigit c || isLetter c) ---
  convert_header_field :: (Header_field, String) -> Header_field' ---
  convert_header_field (name, value) = Header_field' (write_header_field_name name) value ---
  convert_events_to_pitched :: Note Pitched -> [Event note_type] -> [Event Pitched] ---
  convert_events_to_pitched unpitched_note = (<$>) convert_event_to_pitched where ---
    convert_event_to_pitched :: Event note_type -> Event Pitched ---
    convert_event_to_pitched event = ---
      case event of ---
        Event notes len -> Event (convert_notes_to_pitched notes) len ---
        Triplet events -> Triplet (convert_events_to_pitched unpitched_note events) ---
    convert_note_to_pitched :: Note note_type -> Note Pitched ---
    convert_note_to_pitched note = ---
      case note of ---
        Pitched_note _ _ -> note ---
        Unpitched_note -> unpitched_note ---
    convert_notes_to_pitched :: Notes note_type -> Notes Pitched ---
    convert_notes_to_pitched maybe_notes = ---
      case maybe_notes of ---
        Notes notes -> Notes (Set.mapMonotonic convert_note_to_pitched notes) ---
        Tie -> Tie ---
  delete_default_tagline :: Header_field' ---
  delete_default_tagline = Header_field' "tagline" "" ---
  -- | Encodes the score in Lilypond format and writes it to the specified file. ---
  lilypond :: File_path -> Score -> ExceptT Error IO () ---
  lilypond file_path score = ---
    do ---
      written_score <- except (write_score score) ---
      write_file "ly" writeFile File_error file_path written_score ---
  write_accidental :: Accidental -> String ---
  write_accidental accidental = ---
    case accidental of ---
      Flat -> "f" ---
      Natural -> "" ---
      Sharp -> "s" ---
  write_angular_brackets :: String -> String ---
  write_angular_brackets = write_brackets "<" ">" ---
  write_angular_brackets_2 :: String -> String ---
  write_angular_brackets_2 = write_brackets "<<" ">>" ---
  write_basic_length :: Basic_length -> String ---
  write_basic_length len = show (denominator (basic_length_to_fraction len)) ---
  write_brackets :: String -> String -> String -> String ---
  write_brackets left_bracket right_bracket x = left_bracket <> x <> right_bracket ---
  write_clef :: Clef note_type -> String ---
  write_clef clef = ---
    case clef of ---
      Percussion_clef -> "percussion" ---
      Pitched_clef {clef_name, transposition} -> write_clef_name clef_name <> write_clef_transposition transposition ---
  write_clef_name :: Clef_name -> String ---
  write_clef_name clef_name = ---
    case clef_name of ---
      Subbass -> "subbass" ---
      Bass -> "bass" ---
      Baritone_F -> "baritonevarF" ---
      Baritone_C -> "baritone" ---
      Tenor -> "tenor" ---
      Alto -> "alto" ---
      Mezzosoprano -> "mezzosoprano" ---
      Soprano -> "soprano" ---
      Treble -> "treble" ---
      French -> "french" ---
  write_clef_transposition :: Steps -> String ---
  write_clef_transposition transposition = ---
    case compare transposition 0 of ---
      LT -> write_clef_transposition' "_" (negate transposition) ---
      EQ -> "" ---
      GT -> write_clef_transposition' "^" transposition ---
  write_clef_transposition' :: String -> Steps -> String ---
  write_clef_transposition' index transposition = index <> show transposition ---
  write_curly_brackets :: String -> String ---
  write_curly_brackets = write_brackets "{" "}" ---
  write_dot :: Dot -> Char ---
  write_dot Dot = '.' ---
  write_end_bar_line :: String ---
  write_end_bar_line = "\\bar" <-> "\"|.\"" ---
  write_eq :: String -> String -> String ---
  write_eq x y = x <-> "=" <-> y ---
  write_header :: [Header_field'] -> Either Error String ---
  write_header header = ---
    do ---
      written_header_fields <- traverse write_header_field header ---
      Right ("\\header" <-> write_curly_brackets (intercalate " " written_header_fields)) ---
  write_header_field :: Header_field' -> Either Error String ---
  write_header_field (Header_field' name value) = write_eq name <$> write_quotes value ---
  write_header_field_name :: Header_field -> String ---
  write_header_field_name name = ---
    case name of ---
      Composer -> "composer" ---
      Instrument -> "instrument" ---
      Subtitle -> "subtitle" ---
  write_key :: Key -> Either Error String ---
  write_key key = ---
    do ---
      note_names <- ---
        traverse ---
          (write_key_note_name (fromListWith (<>) (second return <$> deconstruct_note_name <$> Set.elems key))) ---
          (enumFromTo C_natural B_natural) ---
      Right (write_note_name C <-> "#" <> "`" <> write_round_brackets (intercalate " " note_names)) ---
  write_key_accidental :: Accidental -> String ---
  write_key_accidental accidental = ---
    case accidental of ---
      Flat -> "FLAT" ---
      Natural -> "NATURAL" ---
      Sharp -> "SHARP" ---
  write_key_natural_note_name :: Natural_note_name -> String ---
  write_key_natural_note_name natural_note_name = show (fromEnum natural_note_name) ---
  write_key_note_name :: Map Natural_note_name [Accidental] -> Natural_note_name -> Either Error String ---
  write_key_note_name key natural_note_name = ---
    case Map.lookup natural_note_name key of ---
      Nothing -> write_key_note_name' natural_note_name Natural ---
      Just accidentals' -> ---
        case accidentals' of ---
          [accidental] -> write_key_note_name' natural_note_name accidental ---
          _ -> Left Conflicting_key_signature ---
  write_key_note_name' :: Natural_note_name -> Accidental -> Either Error String ---
  write_key_note_name' natural_note_name accidental = ---
    Right ---
      (write_round_brackets (write_key_natural_note_name natural_note_name <-> "." <-> "," <> write_key_accidental accidental)) ---
  write_language :: Either Error String ---
  write_language = ---
    do ---
      written_language <- write_quotes "english.ly" ---
      Right ("\\include" <-> written_language) ---
  write_length :: Length -> String ---
  write_length (Length len dots) = write_basic_length len <> (write_dot <$> dots) ---
  write_natural_note_name :: Natural_note_name -> String ---
  write_natural_note_name natural_note_name = ---
    case natural_note_name of ---
      C_natural -> "c" ---
      D_natural -> "d" ---
      E_natural -> "e" ---
      F_natural -> "f" ---
      G_natural -> "g" ---
      A_natural -> "a" ---
      B_natural -> "b" ---
  write_note :: Note Pitched -> String ---
  write_note (Pitched_note octave note_name) = write_note_name note_name <> write_octave octave ---
  write_note_name :: Note_name -> String ---
  write_note_name note_name = ---
    let ---
      (natural_note_name, accidental) = deconstruct_note_name note_name ---
    in ---
      write_natural_note_name natural_note_name <> write_accidental accidental ---
  write_notes :: Notes Pitched -> StateT Composition.Lilypond.State (Either Error) (Bool, String) ---
  write_notes maybe_notes = ---
    case maybe_notes of ---
      Notes notes -> ---
        do ---
          #state_notes .= not (Set.null notes) ---
          return ---
            ( ---
              False, ---
              case Set.elems notes of ---
                [] -> "r" ---
                notes' -> write_angular_brackets (intercalate " " (write_note <$> notes'))) ---
      Tie -> ---
        do ---
          -- Composition.Lilypond.State {state_notes} <- get ---
          notes <- use #state_notes ---
          return ---
            ( ---
              notes, ---
              case notes of ---
                False -> "r" ---
                True -> "") ---
  write_octave :: Octave -> String ---
  write_octave octave = ---
    case compare octave 3 of ---
      LT -> replicate (3 - octave) ',' ---
      EQ -> "" ---
      GT -> replicate (octave - 3) '\'' ---
  write_option :: String -> String -> String ---
  write_option option value = "#" <> write_round_brackets ("ly:set-option" <-> "'" <> option <-> value) ---
  write_options :: String ---
  write_options = intercalate " " [write_option "delete-intermediate-files" "#t", write_option "no-point-and-click" "#t"] ---
  write_override :: String -> String -> String ---
  write_override name value = "\\override" <-> write_eq name value ---
  write_part :: Part -> Either Error String ---
  write_part (Part {title, key, time, initial_position, stave_groups}) = ---
    do ---
      written_header <- write_header [Header_field' "piece" title] ---
      written_set_key <- write_set_key key ---
      written_time_and_initial_position <- write_time_and_initial_position ---
      _ <- tracks_length (remove_notational_information stave_groups) ---
      written_stave_groups <- traverse write_stave_group stave_groups ---
      Right ---
        ( ---
          written_set_key <-> ---
          written_time_and_initial_position <-> ---
          "\\score" <-> ---
          write_curly_brackets (written_header <-> write_angular_brackets_2 (intercalate " " written_stave_groups))) where ---
    write_clef_and_stave :: Clef_and_stave note_type -> Either Error String ---
    write_clef_and_stave (Clef_and_stave {clef, stave}) = ---
      do ---
        written_set_clef <- write_set_clef clef ---
        written_tracks <- ---
          case stave of ---
            One_track events -> write_events (convert_events_to_pitched (Pitched_note 4 C) events) ---
            Two_tracks events_0 events_1 -> ---
              do ---
                written_voice_0 <- write_voice "\\voiceOne" (convert_events_to_pitched (Pitched_note 4 D) events_0) ---
                written_voice_1 <- write_voice "\\voiceTwo" (convert_events_to_pitched (Pitched_note 3 B) events_1) ---
                Right (write_angular_brackets_2 (written_voice_0 <-> written_voice_1)) ---
        Right ---
          ( ---
            "\\new" <-> ---
            "Staff" <-> ---
            "\\with" <-> ---
            write_curly_brackets ---
              (case clef of ---
                Percussion_clef -> ---
                  ( ---
                    write_override "StaffSymbol.line-count" (show (length written_tracks)) <-> ---
                    write_override "Stem.neutral-direction" "1") ---
                Pitched_clef {} -> "") <-> ---
            write_curly_brackets ---
              ( ---
                intercalate ---
                  " " ---
                  ( ---
                    (case clef of ---
                      Percussion_clef -> [] ---
                      Pitched_clef {} -> ["\\Key"]) <> ---
                    ["\\Time_and_initial_position", written_set_clef]) <-> ---
                written_tracks <-> ---
                write_end_bar_line)) ---
    write_event :: Int -> Event Pitched -> StateT Composition.Lilypond.State (Either Error) (Bool, String) ---
    write_event triplets event = ---
      case event of ---
        Event notes len -> ---
          do ---
            Composition.Lilypond.State {state_position} <- get ---
            let position' = state_position + (2 % 3) ^ triplets * length_to_fraction len ---
            position'' <- ---
              case compare position' (measure_length time) of ---
                LT -> return position' ---
                EQ -> return 0 ---
                GT -> throwError An_event_crosses_the_bar_line ---
            #state_position .= position'' ---
            (tie, written_notes) <- write_notes notes ---
            return (tie, written_notes <> write_length len) ---
        Triplet events -> ---
          do ---
            maybe_written_events <- write_events' (1 + triplets) events ---
            case maybe_written_events of ---
              Nothing -> throwError Empty_triplet ---
              Just (tie, written_events) -> return (tie, "\\tuplet" <-> "3/2" <-> write_curly_brackets written_events) ---
    write_events :: [Event Pitched] -> Either Error String ---
    write_events events = ---
      do ---
        maybe_written_events <- ---
          evalStateT ---
            (write_events' 0 events) ---
            (Composition.Lilypond.State {state_notes = False, state_position = initial_position_to_fraction initial_position}) ---
        Right ---
          (case maybe_written_events of ---
            Nothing -> "" ---
            Just (_, written_events) -> written_events) ---
    write_events' :: Int -> [Event Pitched] -> StateT Composition.Lilypond.State (Either Error) (Maybe (Bool, String)) ---
    write_events' triplets events = ---
      case events of ---
        [] -> return Nothing ---
        event : events' -> ---
          do ---
            (tie_0, written_event) <- write_event triplets event ---
            maybe_written_events <- write_events' triplets events' ---
            return ---
              (Just ---
                ( ---
                  tie_0, ---
                  intercalate ---
                    " " ---
                    ( ---
                      written_event : ---
                      case maybe_written_events of ---
                        Nothing -> [] ---
                        Just (tie_1, written_events) -> ---
                          ( ---
                            (case tie_1 of ---
                              False -> [] ---
                              True -> ["~"]) <> ---
                            [written_events])))) ---
    write_initial_position :: Either Error String ---
    write_initial_position = ---
      do ---
        let initial_position' = initial_position_to_fraction initial_position ---
        check
          (Is_out_of_range_without_location Initial_position_IOOR)
          (between 0 (measure_length time - 1 % 128) initial_position') ---
        let num :% den = measure_length time - initial_position' ---
        Right (show den <-> "*" <-> show num) ---
    write_instrument_clefs_and_staves :: Pitched_or_unpitched Instrument_clefs_and_staves -> Either Error String ---
    write_instrument_clefs_and_staves = pitched_and_unpitched write_instrument_clefs_and_staves' ---
    write_instrument_clefs_and_staves' :: Instrument_clefs_and_staves note_type -> Either Error String ---
    write_instrument_clefs_and_staves' (Instrument_clefs_and_staves {instrument_name, clefs_and_staves}) = ---
      do ---
        written_instrument_name <- write_quotes instrument_name ---
        written_clefs_and_staves <- traverse write_clef_and_stave clefs_and_staves ---
        Right ---
          ( ---
            "\\new" <-> ---
            "PianoStaff" <-> ---
            "\\with" <-> ---
            write_curly_brackets (write_eq "instrumentName" written_instrument_name) <-> ---
            write_angular_brackets_2 (intercalate " " written_clefs_and_staves)) ---
    write_set_initial_position :: Either Error String ---
    write_set_initial_position = ---
      do ---
        written_initial_position <- write_initial_position ---
        Right ("\\partial" <-> written_initial_position) ---
    write_stave_group :: [Pitched_or_unpitched Instrument_clefs_and_staves] -> Either Error String ---
    write_stave_group stave_group = ---
      case stave_group of ---
        [instrument_clefs_and_staves] -> write_instrument_clefs_and_staves instrument_clefs_and_staves ---
        _ -> ---
          do ---
            written_instruments_clefs_and_staves <- traverse write_instrument_clefs_and_staves stave_group ---
            Right ("\\new" <-> "StaffGroup" <-> write_angular_brackets_2 (intercalate " " written_instruments_clefs_and_staves)) ---
    write_time_and_initial_position :: Either Error String ---
    write_time_and_initial_position = ---
      do ---
        written_set_initial_position <- write_set_initial_position ---
        Right ( ---
          write_eq ---
            "Time_and_initial_position" ---
            (write_curly_brackets ---
              (intercalate " " (["\\numericTimeSignature", write_set_time time, written_set_initial_position])))) ---
    write_voice :: String -> [Event Pitched] -> Either Error String ---
    write_voice voice_name events = ---
      do ---
        written_events <- write_events events ---
        Right ("\\new" <-> "Voice" <-> write_curly_brackets (voice_name <-> write_curly_brackets written_events)) ---
  write_quotes :: String -> Either Error String ---
  write_quotes text = ---
    do ---
      traverse_ check_char text ---
      Right (write_brackets "\"" "\"" text) ---
  write_round_brackets :: String -> String ---
  write_round_brackets = write_brackets "(" ")" ---
  write_score :: Score -> Either Error String ---
  write_score (Score {title, header, parts}) = ---
    do ---
      written_parts <- traverse write_part parts ---
      written_language <- write_language ---
      written_header <- ---
        write_header (delete_default_tagline : Header_field' "title" title : (convert_header_field <$> assocs header)) ---
      Right (write_options <-> written_language <-> written_header <-> intercalate " " written_parts) ---
  write_set_clef :: Clef note_type -> Either Error String ---
  write_set_clef clef = ---
    do ---
      written_clef <- write_quotes (write_clef clef) ---
      Right ("\\clef" <-> written_clef) ---
  write_set_key :: Key -> Either Error String ---
  write_set_key key = ---
    do ---
      written_key <- write_key key ---
      Right (write_eq "Key" (write_curly_brackets ("\\key" <-> written_key))) ---
  write_set_time :: Time -> String ---
  write_set_time time = "\\time" <-> write_time time ---
  write_time :: Time -> String ---
  write_time (Time num den) = write_time_numerator num <> "/" <> write_basic_length den ---
  write_time_numerator :: [Time_numerator_factor] -> String ---
  write_time_numerator num = show (time_numerator_to_int num) ---