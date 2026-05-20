{-|
Write the custom file format.
-}
module Composition.Write (write, write_accidental, write_natural_note_name, write_note) where
  import Composition.Errors
  import Composition.Notation
  import Composition.Notes
  import Composition.Score
  import Control.Monad.Except
  import Data.List as List
  import Data.Map.Strict
  import Data.Maybe
  import Data.Set as Set
  import Parser.Files
  import Parser.Utilities
  -- | Encode the score in .aoi format and write it to the specified file.
  write :: File_path -> Score -> ExceptT Error IO ()
  write file_path score = write_file "aoi" writeFile File_error file_path (write_score score)
  -- | Write an accidental. Mostly for internal use.
  write_accidental :: Accidental -> String
  write_accidental accidental = fromJust (List.lookup accidental accidentals)
  write_basic_length :: Basic_length -> String
  write_basic_length len = show (basic_length_denominator len)
  write_brackets :: String -> String -> String -> String
  write_brackets left_bracket right_bracket x = left_bracket <> x <> right_bracket
  write_curly_brackets :: String -> String
  write_curly_brackets = write_brackets "{" "}"
  write_dot :: Dot -> Char
  write_dot Dot = '.'
  write_eq :: String -> String -> String
  write_eq x y = x <-> "=" <-> y
  write_event :: Event note_type -> String
  write_event event =
    case event of
      Event notes len -> write_notes notes <> write_length len
      Triplet events -> write_events events
  write_events :: [Event note_type] -> String
  write_events = write_list write_event
  write_header_field :: (Header_field, String) -> String
  write_header_field (name, value) = write_eq (show name) (write_quotes value)
  write_initial_position :: Initial_position -> String
  write_initial_position (Initial_position num den) =
    case num of
      0 -> "0"
      _ -> show num <> "/" <> write_basic_length den
  write_instrument_and_staves :: Instrument_and_staves note_type -> String
  write_instrument_and_staves
    (Instrument_and_staves {instrument_name, short_instrument_name, midi_instrument, velocity, staves}) =
      write_struct
        "Instrument_and_staves"
        (
          write_eq "instrument_name" (write_quotes instrument_name) <->
          write_eq "short_instrument_name" (write_quotes short_instrument_name) <->
          write_eq "midi_instrument" (show midi_instrument) <->
          write_eq "velocity" (show velocity) <->
          write_eq "staves" (write_list write_stave staves))
  write_length :: Length -> String
  write_length (Length len dots) = write_basic_length len <> (write_dot <$> dots)
  write_list :: (t -> String) -> [t] -> String
  write_list write_t x = write_square_brackets (intercalate " " (write_t <$> x))
  -- | Write a natural note name. Mostly for internal use.
  write_natural_note_name :: Natural_note_name -> String
  write_natural_note_name natural_note_name = [head (show natural_note_name)]
  write_note :: Note note_type -> String
  write_note note =
    case note of
      Pitched_note octave note_name -> write_note_name note_name <> show octave
      Unpitched_note -> "x"
  -- | Write a note name. Mostly for internal use.
  write_note_name :: Note_name -> String
  write_note_name note_name =
    let
      (natural_note_name, accidental) = deconstruct_note_name note_name in
      write_natural_note_name natural_note_name <> write_accidental accidental
  write_notes :: Notes note_type -> String
  write_notes maybe_notes =
    case maybe_notes of
      Notes notes -> write_list write_note (Set.elems notes)
      Tie -> "+"
  write_part :: Part -> String
  write_part (Part {title, key, time, initial_position, tempo, stave_groups}) =
    write_struct
      "Part"
      (
        write_eq "title" (write_quotes title) <->
        write_eq "key" write_key <->
        write_eq "time" (write_time time) <->
        write_eq "initial_position" (write_initial_position initial_position) <->
        write_eq "tempo" (show tempo) <->
        write_eq
          "stave_groups"
          (write_list (write_list (write_pitched_or_unpitched write_instrument_and_staves)) stave_groups)) where
    write_key :: String
    write_key = write_list write_note_name (Set.elems key)
  write_pitched_or_unpitched :: (forall note_type . f note_type -> String) -> Pitched_or_unpitched f -> String
  write_pitched_or_unpitched write_f x =
    pitched_or_unpitched (\ _ -> "Pitched") (\ _ -> "Unpitched") x <-> pitched_and_unpitched write_f x
  write_quotes :: String -> String
  write_quotes = write_brackets "\"" "\""
  write_score :: Score -> String
  write_score (Score {header, parts}) =
    write_struct "Score" (write_eq "header" write_header <-> write_eq "parts" (write_list write_part parts)) where
    write_header :: String
    write_header = write_list write_header_field (assocs header)
  write_square_brackets :: String -> String
  write_square_brackets = write_brackets "[" "]"
  write_stave :: Stave note_type -> String
  write_stave (Stave {clef, tracks}) =
    write_struct "Stave" (write_eq "clef" write_clef <-> write_eq "tracks" write_tracks) where
    write_clef :: String
    write_clef =
      case clef of
        Pitched_clef {clef_name, transposition} ->
          write_struct "Pitched_clef" (write_eq "clef_name" (show clef_name) <-> write_eq "transposition" (show transposition))
        Percussion_clef -> "Percussion_clef"
    write_tracks :: String
    write_tracks = write_list write_events (tracks_to_list tracks)
  write_struct :: String -> String -> String
  write_struct name fields = name <-> write_curly_brackets fields
  write_time :: Time -> String
  write_time (Time num den) = "Time" <-> write_list write_time_numerator_factor num <-> write_basic_length den
  write_time_numerator_factor :: Time_numerator_factor -> String
  write_time_numerator_factor time_numerator_factor = show (time_numerator_factor_to_int time_numerator_factor)