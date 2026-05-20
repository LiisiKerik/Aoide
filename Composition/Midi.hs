{-|
Generates MIDI files.
-}
module Composition.MIDI (midi) where
  import Composition.Errors
  import Composition.Notation
  import Composition.Notes
  import Composition.Score
  import Composition.Theory
  import Control.Monad
  import Control.Monad.Except
  import Control.Monad.State.Strict
  import Control.Monad.Trans.Except
  import Data.ByteString as ByteString
  import Data.Foldable as Foldable
  import Data.List as List
  import Data.Ratio
  import Data.Set as Set
  import Data.Word
  import Parser.Files
  import Parser.Utilities
  data MIDI_part = MIDI_part {time_denominator :: Basic_length, tempo :: Tempo, tracks :: [Pitched_or_unpitched Track]}
  deriving instance Show MIDI_part
  encode_chunk :: [Word8] -> [Word8] -> Either Error [Word8]
  encode_chunk typ dat =
    do
      len <- encode_int_fixed 4 Track_length_IOOR (Foldable.length dat)
      Right (typ <> len <> dat)
  encode_format :: [Word8]
  encode_format = [0, 1]
  encode_int_fixed :: (MonadError Error f) => Integer -> Is_out_of_range_type -> Int -> f [Word8]
  encode_int_fixed bytes typ i =
    case bytes of
      0 ->
        case i of
          0 -> return []
          _ -> throwError (Is_out_of_range_without_location typ)
      _ ->
        do
          let (i', last_byte) = divMod i 256
          encoded_i' <- encode_int_fixed (bytes - 1) typ i'
          return (encoded_i' <> [fromIntegral last_byte])
  encode_int_flexible :: (MonadError Error f) => Int -> f [Word8]
  encode_int_flexible = encode_int_flexible' 4 0
  encode_int_flexible' :: (MonadError Error f) => Integer -> Word8 -> Int -> f [Word8]
  encode_int_flexible' max_bytes msb i =
    case max_bytes of
      0 -> throwError (Event_or_part_length_is_out_of_range_for_MIDI)
      _ ->
        do
          let (i', last_byte) = divMod i 128
          encoded_i' <-
            case i' of
              0 -> return []
              _ -> encode_int_flexible' (max_bytes - 1) 128 i'
          return (encoded_i' <> [msb + fromIntegral last_byte])
  encode_score :: [MIDI_part] -> Either Error [Word8]
  encode_score parts =
    do
      header <- encode_header
      encoded_parts <- traverse encode_part parts
      tracks <- traverse encode_complete_track (List.transpose encoded_parts)
      Right (header <> join tracks) where
    encode_complete_track :: [[Word8]] -> Either Error [Word8]
    encode_complete_track track_parts =
      do
        end_track <- encode_end_track
        encode_chunk [77, 84, 114, 107] (join track_parts <> end_track)
    encode_end_track :: Either Error [Word8]
    encode_end_track = encode_metaevent 0 47 []
    encode_events :: forall note_type .
      MIDI_instrument -> Word8 -> Velocity -> [Event_fraction note_type] -> StateT [Word8] (Either Error) [Word8]
    encode_events instrument channel velocity events =
      do
        check (Is_out_of_range_without_location Velocity_IOOR) (between 0 max_velocity velocity)
        check (MIDI_instrument_code_is_out_of_range_data (fromIntegral instrument)) (between 0 127 instrument)
        join <$> traverse encode_event (events <> [Event' Set.empty rest_after_part]) where
      encode_event :: Event_fraction note_type -> StateT [Word8] (Either Error) [Word8]
      encode_event (Event' notes len) =
        do
          encoded_notes <- traverse encode_note (elems notes)
          notes_on <- traverse encode_note_on encoded_notes
          rest <- encode_rest
          notes_off <- traverse encode_note_off encoded_notes
          return (join notes_on <> rest <> join notes_off) where
        encode_rest :: StateT [Word8] (Either Error) [Word8]
        encode_rest = encode_metaevent len 1 []
      encode_note :: Note note_type -> StateT [Word8] (Either Error) Word8
      encode_note note =
        case note of
          Pitched_note _ _ ->
            do
              check (Note_is_out_of_range_for_MIDI note) (between min_note max_note note)
              return (fromIntegral (distance_in_semitones min_note note))
          Unpitched_note -> return instrument
      encode_note_event :: Word8 -> Word8 -> StateT [Word8] (Either Error) [Word8]
      encode_note_event typ note = encode_midi_event channel typ [note, velocity]
      encode_note_off :: Word8 -> StateT [Word8] (Either Error) [Word8]
      encode_note_off = encode_note_event 8
      encode_note_on :: Word8 -> StateT [Word8] (Either Error) [Word8]
      encode_note_on = encode_note_event 9
    encode_metaevent :: (MonadError Error f) => Ratio Int -> Word8 -> [Word8] -> f [Word8]
    encode_metaevent time typ dat = encode_midi_or_metaevent time ([255, typ, fromIntegral (Foldable.length dat)] <> dat)
    encode_header :: Either Error [Word8]
    encode_header =
      do
        encoded_number_of_tracks <- encode_int_fixed 2 The_number_of_tracks_IOOR number_of_tracks
        length_of_quarter_note_in_ticks <-
          encode_int_fixed 2 The_least_common_denominator_of_note_lengths_IOOR (length_in_ticks (1 % 4))
        encode_chunk [77, 84, 104, 100] (encode_format <> encoded_number_of_tracks <> length_of_quarter_note_in_ticks)
    encode_midi_event :: Word8 -> Word8 -> [Word8] -> StateT [Word8] (Either Error) [Word8]
    encode_midi_event channel typ dat = encode_midi_or_metaevent 0 ([channel + 16 * typ] <> dat)
    encode_midi_or_metaevent :: (MonadError Error f) => Ratio Int -> [Word8] -> f [Word8]
    encode_midi_or_metaevent time event =
      do
        encoded_time <- encode_length
        return (encoded_time <> event) where
      encode_length :: (MonadError Error f) => f [Word8]
      encode_length = encode_int_flexible (length_in_ticks time)
    encode_part :: MIDI_part -> Either Error [[Word8]]
    encode_part (MIDI_part {time_denominator, tempo, tracks}) =
      do
        encoded_tempo <- encode_tempo
        empty_track <- create_empty_track
        encoded_tracks <- encode_tracks (tracks <> List.replicate (number_of_tracks - Foldable.length tracks) empty_track)
        Right
          (case encoded_tracks of
            [] -> []
            track : tracks' -> (encoded_tempo <> track) : tracks') where
      create_empty_track :: Either Error (Pitched_or_unpitched Track)
      create_empty_track =
        do
          len <- tracks_length tracks
          Right (Unpitched (Track {instrument = 0, velocity = 0, events = [Event' Set.empty len]}))
      encode_tempo :: Either Error [Word8]
      encode_tempo =
        do
          encoded_tempo <- encode_int_fixed 3 Tempo_IOOR quarter_note_length_in_microseconds
          encode_metaevent 0 81 encoded_tempo
      quarter_note_length_in_microseconds :: Int
      quarter_note_length_in_microseconds = round ((60000000 * basic_length_denominator time_denominator) % (4 * tempo))
    encode_pitched_track :: Track Pitched -> StateT [Word8] (Either Error) [Word8]
    encode_pitched_track (Track {instrument, velocity, events}) =
      do
        channel <- new_channel
        encoded_instrument <- encode_instrument channel
        encoded_events <- encode_events instrument channel velocity events
        return (encoded_instrument <> encoded_events) where
      encode_instrument :: Word8 -> StateT [Word8] (Either Error) [Word8]
      encode_instrument channel = encode_midi_event channel 12 [instrument]
    encode_track :: Pitched_or_unpitched Track -> StateT [Word8] (Either Error) [Word8]
    encode_track = pitched_or_unpitched encode_pitched_track encode_unpitched_track
    encode_tracks :: [Pitched_or_unpitched Track] -> Either Error [[Word8]]
    encode_tracks tracks = evalStateT (traverse encode_track tracks) pitched_channels
    encode_unpitched_track :: Track Unpitched -> StateT [Word8] (Either Error) [Word8]
    encode_unpitched_track (Track {instrument, velocity, events}) = encode_events instrument unpitched_channel velocity events
    lcd :: Int
    lcd = 4 `lcm` lcm_all (lcd_part <$> parts)
    length_in_ticks :: Length_fraction -> Int
    length_in_ticks len = numerator (fromIntegral lcd * len)
    number_of_tracks :: Int
    number_of_tracks = Foldable.maximum (0 : (number_of_tracks_in_part <$> parts))
  lcd_event :: Event_fraction note_type -> Int
  lcd_event (Event' _ len) = denominator len
  lcd_part :: MIDI_part -> Int
  lcd_part (MIDI_part {time_denominator, tracks}) =
    basic_length_denominator time_denominator `lcm` lcm_all (pitched_and_unpitched lcd_track <$> tracks)
  lcd_track :: Track note_type -> Int
  lcd_track (Track {events}) = lcm_all (lcd_event <$> events)
  max_note :: Note Pitched
  max_note = Pitched_note 9 G
  -- | Encode the score in MIDI format and write it to the specified file.
  midi :: Score -> File_path -> ExceptT Error IO ()
  midi score file_path =
    do
      encoded_score <- except (encode_score (midi_score score))
      write_file "mid" ByteString.writeFile File_error file_path (pack encoded_score) where
  midi_score :: Score -> [MIDI_part]
  midi_score (Score {parts}) = midi_part <$> parts
  midi_part :: Part -> MIDI_part
  midi_part (Part {time = Time _ time_denominator, tempo, stave_groups}) =
    MIDI_part {time_denominator, tempo, tracks = remove_notational_information stave_groups}
  min_note :: Note Pitched
  min_note = Pitched_note -1 C
  new_channel :: StateT [Word8] (Either Error) Word8
  new_channel =
    do
      channels <- get
      case channels of
        [] -> throwError (Is_out_of_range_without_location The_number_of_pitched_tracks_IOOR)
        channel : channels' ->
          do
            put channels'
            return channel
  number_of_tracks_in_part :: MIDI_part -> Int
  number_of_tracks_in_part (MIDI_part {tracks}) = Foldable.length tracks
  pitched_channels :: [Word8]
  pitched_channels = List.delete unpitched_channel [0 .. 15]
  rest_after_part :: Length_fraction
  rest_after_part = 1
  unpitched_channel :: Word8
  unpitched_channel = 9