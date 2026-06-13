{-|
Errors.
-}
module Composition.Errors (Error (..), Is_out_of_range_type (..)) where
  import Composition.Notes
  import Parser.Files
  import Parser.Locations
  -- | Errors.
  data Error =
    An_event_crosses_the_bar_line |
    Conflicting_accidentals_in_key_signature |
    Duplicate_header_fields (Maybe Location) |
    Duplicate_keys Location |
    Duplicate_note_names_in_key_signature Location |
    Duplicate_notes Location |
    Empty_triplet |
    Event_or_part_length_is_out_of_range_for_MIDI |
    File_error File_error |
    Initial_position_is_out_of_range |
    Invalid_character_in_text |
    Invalid_keyword_playability String Location |
    Invalid_note_length Int Location |
    Invalid_note_name Natural_note_name Accidental Location |
    Invalid_time_numerator_factor Int Location |
    Invalid_word_command String |
    Invalid_word_score String Location |
    Is_out_of_range_with_location Is_out_of_range_type Location |
    Is_out_of_range_without_location Is_out_of_range_type |
    Key_is_out_of_range Int Location |
    MIDI_instrument_code_is_out_of_range_command Int |
    MIDI_instrument_code_is_out_of_range_data Int |
    MIDI_instrument_code_is_out_of_range_score Int Location |
    Non_positive_note_length |
    Note_is_out_of_range_for_MIDI (Note Pitched) |
    Note_length_denominator_contains_factors_other_than_2_and_3 |
    Parse_error_command |
    Parse_error_playability Location |
    Parse_error_score Location |
    Track_ends_with_an_incomplete_triplet |
    Track_length_mismatch |
    Velocity_out_of_range Location
  data Is_out_of_range_type =
    Note_length_denominator_IOOR |
    Tempo_IOOR |
    The_least_common_denominator_of_note_lengths_IOOR |
    The_number_of_pitched_tracks_IOOR |
    The_number_of_tracks_IOOR |
    Track_length_IOOR |
    Velocity_IOOR
  deriving instance Show Error
  deriving instance Show Is_out_of_range_type