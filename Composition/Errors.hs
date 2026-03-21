{-|
Description: Errors.
-}
module Composition.Errors (Error (..), Is_out_of_range_type (..), write_error) where
  import Parser.Files
  import Parser.Locations
  import Parser.Utilities
  -- | Errors.
  data Error =
    An_event_crosses_the_bar_line | ---
    Conflicting_key_signature | ---
    Duplicate_header_fields Location | ---
    Duplicate_keys Location | ---
    Duplicate_note_names_in_key_signature Location | ---
    Duplicate_notes Location | ---
    Empty_triplet | ---
    File_error File_error | ---
    Invalid_character_in_text | ---
    Invalid_keyword String | ---
    Invalid_note_length Location | ---
    Invalid_note_name Location | ---
    Invalid_time_numerator_factor Location | ---
    Invalid_word String Location | ---
    Is_out_of_range_with_location Is_out_of_range_type Location |
    Is_out_of_range_without_location Is_out_of_range_type |
    Non_positive_note_length | ---
    Note_length_denominator_contains_factors_other_than_2_and_3 | ---
    Parse_error Location | ---
    Track_ends_with_an_incomplete_triplet | ---
    Track_length_mismatch | ---
    Velocity_out_of_range Location ---
  -- | Out of range error types.
  data Is_out_of_range_type =
    Event_or_part_length_IOOR |
    Initial_position_IOOR |
    Key_IOOR |
    MIDI_instrument_code_IOOR |
    Note_IOOR |
    Note_length_denominator_IOOR | ---
    Tempo_IOOR |
    The_least_common_denominator_of_note_lengths_IOOR |
    The_number_of_pitched_tracks_IOOR |
    The_number_of_tracks_IOOR |
    Track_length_IOOR |
    Velocity_IOOR
  deriving instance Show Error
  deriving instance Show Is_out_of_range_type
  -- | Writing errors. ---
  write_error :: Error -> String ---
  write_error err = ---
    case err of ---
      An_event_crosses_the_bar_line -> "An event crosses the bar line." ---
      Conflicting_key_signature -> "Conflicting key signature." ---
      Duplicate_header_fields location -> "Duplicate header fields at " <> write_location location <> "." ---
      Duplicate_keys location -> "Duplicate keys at " <> write_location location <> "." ---
      Duplicate_note_names_in_key_signature location -> ---
        "Duplicate note names in key signature at " <> write_location location <> "." ---
      Duplicate_notes location -> "Duplicate notes at " <> write_location location <> "." ---
      Empty_triplet -> "Empty triplet." ---
      File_error err' -> ---
        case err' of ---
          Failed_to_find_the_file file_path -> "Failed to find the file " <> file_path <> "." ---
          Invalid_file_path -> "Invalid file path." ---
          Unexpected_extension ext -> "Unexpected extension ." <> ext <> "." ---
      Invalid_character_in_text -> "Invalid character in text." ---
      Invalid_keyword keyword -> "Invalid keyword " <> keyword <> "." ---
      Invalid_note_length location -> "Invalid note length at " <> write_location location <> "." ---
      Invalid_note_name location -> "Invalid note name at " <> write_location location <> "."
      Invalid_time_numerator_factor location -> "Invalid time numerator at " <> write_location location <> "." ---
      Invalid_word word location -> "Invalid word " <> word <> " at " <> write_location location <> "." ---
      Is_out_of_range_with_location typ location ->
        write_is_out_of_range_type typ <-> "at" <-> write_location location <-> "is out of range."
      Is_out_of_range_without_location typ -> write_is_out_of_range_type typ <-> "is out of range."
      Non_positive_note_length -> "Non-positive note length." ---
      Note_length_denominator_contains_factors_other_than_2_and_3 -> ---
        "Note length denominator contains factors other than 2 and 3." ---
      Parse_error location -> "Parse error at " <> write_location location <> "." ---
      Track_ends_with_an_incomplete_triplet -> "Track ends with an incomplete triplet." ---
      Track_length_mismatch -> "Track length mismatch." ---
      Velocity_out_of_range location -> "Velocity out of range at " <> write_location location <> "." ---
  write_is_out_of_range_type :: Is_out_of_range_type -> String
  write_is_out_of_range_type typ =
    case typ of
      Event_or_part_length_IOOR -> "Event or part length"
      Initial_position_IOOR -> "Initial position"
      Key_IOOR -> "Key"
      MIDI_instrument_code_IOOR -> "MIDI instrument code"
      Note_IOOR -> "Note"
      Note_length_denominator_IOOR -> "Note length denominator"
      Tempo_IOOR -> "Tempo"
      The_least_common_denominator_of_note_lengths_IOOR -> "The least common denominator of note lengths"
      The_number_of_pitched_tracks_IOOR -> "The number of pitched tracks"
      The_number_of_tracks_IOOR -> "The number of tracks"
      Track_length_IOOR -> "Track length"
      Velocity_IOOR -> "Velocity"