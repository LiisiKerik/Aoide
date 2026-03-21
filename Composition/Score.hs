{-| ---
Description: Lilypond + MIDI capable scores. ---
 ---
* Lilypond + MIDI capable scores ---
-} ---
module Composition.Score ( ---
  Clef (..), ---
  Clef_and_stave (..), ---
  Clef_name (..), ---
  Header_field (..), ---
  Instrument_clefs_and_staves (..), ---
  Key, ---
  MIDI_instrument, ---
  Part (..), ---
  Score (..), ---
  Stave (..), ---
  Tempo, ---
  Track (..), ---
  Velocity, ---
  max_velocity, ---
  notate, ---
  remove_notational_information, ---
  stave_to_events, ---
  tracks_length) where ---
  import Composition.Errors ---
  import Composition.Notes ---
  import Composition.Theory ---
  import Composition.Time ---
  import Control.Monad ---
  import Data.Fixed ---
  import Data.Foldable ---
  import Data.Functor.Barbie ---
  import Data.Map.Strict ---
  import Data.Maybe ---
  import Data.Ratio ---
  import Data.Set as Set ---
  import Data.Word ---
  import GHC.Real ---
  import Parser.Utilities ---
  -- | Clefs (with transposition). ---
  data Clef note_type where ---
    Pitched_clef :: {clef_name :: Clef_name, transposition :: Steps} -> Clef Pitched ---
    Percussion_clef :: Clef Unpitched ---
  -- | Clef names. ---
  data Clef_name = Subbass | Bass | Baritone_F | Baritone_C | Tenor | Alto | Mezzosoprano | Soprano | Treble | French ---
  -- | Stave with clef. ---
  data Clef_and_stave note_type = Clef_and_stave {clef :: Clef note_type, stave :: Stave note_type} ---
  -- Score header fields. ---
  data Header_field = Composer | Instrument | Subtitle ---
  -- | A group of staves for one instrument. ---
  data Instrument_clefs_and_staves note_type = ---
    Instrument_clefs_and_staves { ---
      instrument_name :: String, ---
      midi_instrument :: MIDI_instrument, ---
      velocity :: Velocity, ---
      clefs_and_staves :: [Clef_and_stave note_type]} ---
  -- | A set of note names that serves as a key signature. For example, the key signature of C minor would be @[E_flat, A_flat, ---
  -- B_flat]@. Naturals and conflicting accidentals will result in an error. ---
  type Key = Set Note_name ---
  -- MIDI instrument code. ---
  type MIDI_instrument = Word8 ---
  data Notate_notes note_type = Notate_rest | Notate_notes (Set (Note note_type)) | Notate_tie ---
  -- | Each part can have a different key, time signature and instrumentation. ---
  data Part = ---
    Part { ---
      title :: String, ---
      key :: Key, ---
      time :: Time, ---
      initial_position :: Initial_position, ---
      tempo :: Tempo, ---
      stave_groups :: [[Pitched_or_unpitched Instrument_clefs_and_staves]]} ---
  -- A Lilypond + MIDI capable score. ---
  data Score = Score {title :: String, header :: Map Header_field String, parts :: [Part]} ---
  -- | A stave with one or two voices. Rhythmically independent voices require separate tracks. ---
  data Stave note_type = One_track [Event note_type] | Two_tracks [Event note_type] [Event note_type] ---
  -- | Tempo in beats per minute. ---
  type Tempo = Int ---
  -- | Tracks stripped of notational information (brackets, instrument names, clefs and note length display information). Mostly ---
  -- for internal use. ---
  data Track note_type = Track {instrument :: MIDI_instrument, velocity :: Velocity, events :: [Event_fraction note_type]} ---
  -- | MIDI volume ranges from 0 to 127. ---
  type Velocity = Word8 ---
  deriving instance Eq Clef_name ---
  deriving instance Eq Header_field ---
  deriving instance Ord Clef_name ---
  deriving instance Ord Header_field ---
  deriving instance Read Clef_name
  deriving instance Read Header_field
  deriving instance Show (Clef note_type) ---
  deriving instance Show (Clef_and_stave note_type) ---
  deriving instance Show Clef_name ---
  deriving instance Show Header_field ---
  deriving instance Show (Instrument_clefs_and_staves note_type) ---
  deriving instance Show (Notate_notes note_type) ---
  deriving instance Show Part ---
  deriving instance Show Score ---
  deriving instance Show (Stave note_type) ---
  deriving instance Show (Track note_type) ---
  check_length :: Event' notes -> Either Error () ---
  check_length (Event' _ (num :% den)) = ---
    do ---
      check Non_positive_note_length (0 < num) ---
      check Note_length_denominator_contains_factors_other_than_2_and_3 (is_smooth [2, 3] den) ---
      return () ---
  is_smooth :: [Int] -> Int -> Bool ---
  is_smooth factors i = ---
    case i of ---
      1 -> True ---
      _ -> ---
        case factors of ---
          [] -> False ---
          factor : factors' -> is_smooth factors' (remove_factor factor i) ---
  join_events :: [Event' (Notes note_type)] -> [Event_fraction note_type] ---
  join_events events = ---
    case events of ---
      [] -> [] ---
      Event' notes len : events' -> join_events' (Event' (notes_to_set notes) len) events' ---
  join_events' :: Event_fraction note_type -> [Event' (Notes note_type)] -> [Event_fraction note_type] ---
  join_events' (Event' notes_0 len_0) events = ---
    case events of ---
      [] -> [Event' notes_0 len_0] ---
      Event' maybe_notes_1 len_1 : events' -> ---
        case join_notes notes_0 maybe_notes_1 of ---
          Nothing -> join_events' (Event' notes_0 (len_0 + len_1)) events' ---
          Just notes_1' -> Event' notes_0 len_0 : join_events' (Event' notes_1' len_1) events' ---
  join_notes :: Set (Note note_type) -> Notes note_type -> Maybe (Set (Note note_type)) ---
  join_notes notes_0 maybe_notes_1 = ---
    case maybe_notes_1 of ---
      Notes notes_1 -> ---
        case (Set.elems notes_0, Set.elems notes_1) of ---
          ([], []) -> Nothing ---
          _ -> Just notes_1 ---
      Tie -> Nothing ---
  -- | Maximum MIDI volume. ---
  max_velocity :: Velocity ---
  max_velocity = 127 ---
  -- | Add note length display information to events. Mostly for internal use. ---
  notate :: Time -> Initial_position -> [Event_fraction note_type] -> Either Error [Event note_type] ---
  notate time initial_position events = ---
    do ---
      traverse_ check_length events ---
      notate_events time (initial_position_to_fraction initial_position) (transform_event <$> events) ---
  notate_basic_length :: Length_fraction -> Maybe Basic_length ---
  notate_basic_length (num :% den) = ---
    case num of ---
      1 -> ---
        case den of ---
          1 -> Just Whole ---
          2 -> Just Half ---
          4 -> Just Quarter ---
          8 -> Just Eighth ---
          16 -> Just Sixteenth ---
          32 -> Just Thirty_second ---
          64 -> Just Sixty_fourth ---
          128 -> Just One_hundred_and_twenty_eighth ---
          _ -> Nothing ---
      _ -> Nothing ---
  notate_binary_block :: Time -> (Position, [Event' (Notate_notes note_type)]) -> Either Error [Event note_type] ---
  notate_binary_block time (position, events) = ---
    case events of ---
      [event] -> notate_event time position event ---
      _ -> ---
        let ---
          num :% den = events_length events in ---
          ( ---
            (\ evs -> [Triplet evs]) <$> ---
            case num of ---
              1 -> ---
                case denominator_to_basic_length (2 * den) of ---
                  Nothing -> Left (Is_out_of_range_without_location Note_length_denominator_IOOR) ---
                  Just den' -> notate_measure (Time [Three] den') (0, scale_lengths (3 % 2) events) ---
              _ -> ---
                do ---
                  time' <- time_subdivision time ---
                  notate_events time' (mod' position (measure_length time')) events) ---
  notate_event :: Time -> Position -> Event' (Notate_notes note_type) -> Either Error [Event note_type] ---
  notate_event time position event@(Event' notes len) = ---
    case notate_simple_length len of ---
      Nothing -> ---
        do ---
          time' <- time_subdivision time ---
          notate_events time' (mod' position (measure_length time')) [event] ---
      Just len' -> Right [Event (notate_notes notes) len'] ---
  notate_events :: Time -> Position -> [Event' (Notate_notes note_type)] -> Either Error [Event note_type] ---
  notate_events time position events = join <$> traverse (notate_measure time) (separate_measures time position events) ---
  notate_measure :: Time -> (Position, [Event' (Notate_notes note_type)]) -> Either Error [Event note_type] ---
  notate_measure time (position, events) = ---
    do ---
      binary_blocks <- separate_binary_blocks time position events ---
      join <$> traverse (notate_binary_block time) binary_blocks ---
  notate_notes :: Notate_notes note_type -> Notes note_type ---
  notate_notes notes = ---
    case notes of ---
      Notate_rest -> Notes Set.empty ---
      Notate_notes notes' -> Notes notes' ---
      Notate_tie -> Tie ---
  notate_simple_length :: Length_fraction -> Maybe Length ---
  notate_simple_length (num :% den) = ---
    do ---
      dots <- (\ i -> i - 1) <$> lg (1 + num) ---
      basic_length <- notate_basic_length (2 ^ dots % den) ---
      Just (Length basic_length (replicate dots Dot)) ---
  notes_tie :: Notate_notes note_type -> Notate_notes note_type ---
  notes_tie notes = ---
    case notes of ---
      Notate_rest -> Notate_rest ---
      Notate_notes _ -> Notate_tie ---
      Notate_tie -> Notate_tie ---
  notes_to_set :: Notes note_type -> Set (Note note_type) ---
  notes_to_set maybe_notes = ---
    case maybe_notes of ---
      Notes notes -> notes ---
      Tie -> Set.empty ---
  remove_factor :: Int -> Int -> Int ---
  remove_factor factor i = ---
    case mod i factor of ---
      0 -> remove_factor factor (div i factor) ---
      _ -> i ---
  -- | Strip tracks of notational information (brackets, instrument names, clefs, note length display information). Mostly for ---
  -- internal use. ---
  remove_notational_information :: [[Pitched_or_unpitched Instrument_clefs_and_staves]] -> [Pitched_or_unpitched Track] ---
  remove_notational_information stave_groups = ---
    stave_groups >>= flip (>>=) (btraverse remove_notational_information_instrument_clefs_and_staves) ---
  remove_notational_information_clef_and_stave :: Clef_and_stave note_type -> [[Event_fraction note_type]] ---
  remove_notational_information_clef_and_stave (Clef_and_stave {stave}) = ---
    remove_notational_information_events <$> stave_to_events stave ---
  remove_notational_information_events :: [Event note_type] -> [Event_fraction note_type] ---
  remove_notational_information_events events = join_events (remove_notational_information_events' events) ---
  remove_notational_information_events' :: [Event note_type] -> [Event' (Notes note_type)] ---
  remove_notational_information_events' events = events >>= remove_notational_information_event ---
  remove_notational_information_event :: Event note_type -> [Event' (Notes note_type)] ---
  remove_notational_information_event event = ---
    case event of ---
      Event notes len -> [Event' notes (length_to_fraction len)] ---
      Triplet events -> scale_lengths (2 % 3) (remove_notational_information_events' events) ---
  remove_notational_information_instrument_clefs_and_staves :: Instrument_clefs_and_staves note_type -> [Track note_type] ---
  remove_notational_information_instrument_clefs_and_staves ---
    (Instrument_clefs_and_staves {midi_instrument, velocity, clefs_and_staves}) = ---
      ( ---
        (\ events -> Track {instrument = midi_instrument, velocity, events}) <$> ---
        (clefs_and_staves >>= remove_notational_information_clef_and_stave)) ---
  separate_binary_blocks :: ---
    Time -> Position -> [Event' (Notate_notes note_type)] -> Either Error [(Position, [Event' (Notate_notes note_type)])] ---
  separate_binary_blocks time position events = ---
    case events of ---
      [] -> Right [] ---
      event@(Event' _ len) : events' -> ---
        do ---
          let position' = position + len ---
          case is_smooth [2] (denominator position') of ---
            False -> ---
              do ---
                binary_blocks <- separate_binary_blocks time position' events' ---
                case binary_blocks of ---
                  [] -> Left Track_ends_with_an_incomplete_triplet ---
                  (_, events'') : binary_blocks' -> Right ((position, event : events'') : binary_blocks') ---
            True -> (:) (position, [event]) <$> separate_binary_blocks time position' events' ---
  separate_measures :: ---
    Time -> Position -> [Event' (Notate_notes note_type)] -> [(Length_fraction, [Event' (Notate_notes note_type)])] ---
  separate_measures time initial_position events = ---
    case separate_measures' time initial_position events of ---
      [] -> [] ---
      measure : measures' -> (initial_position, measure) : ((,) 0 <$> measures') ---
  separate_measures' :: Time -> Position -> [Event' (Notate_notes note_type)] -> [[Event' (Notate_notes note_type)]] ---
  separate_measures' time position events = ---
    case events of ---
      [] -> [] ---
      Event' notes len : events' -> ---
        let ---
          position' = position + len in ---
          case compare position' (measure_length time) of ---
            LT -> ---
              case separate_measures' time position' events' of ---
                [] -> [[Event' notes len]] ---
                measure : measures -> (Event' notes len : measure) : measures ---
            EQ -> [Event' notes len] : separate_measures' time 0 events' ---
            GT -> ---
              let ---
                len' = measure_length time - position in ---
                [Event' notes len'] : separate_measures' time 0 (Event' (notes_tie notes) (len - len') : events') ---
  -- | Convert the stave to a list of voices. Mostly for internal use. ---
  stave_to_events :: Stave note_type -> [[Event note_type]] ---
  stave_to_events stave = ---
    case stave of ---
      One_track events -> [events] ---
      Two_tracks events_0 events_1 -> [events_0, events_1] ---
  track_length :: Track note_type -> Ratio Int ---
  track_length (Track {events}) = events_length events ---
  -- | Calculate part length. Returns Nothing if there is a track length mismatch. ---
  tracks_length :: [Pitched_or_unpitched Track] -> Either Error Length_fraction ---
  tracks_length tracks = ---
    case all_equal (pitched_and_unpitched track_length <$> tracks) of ---
      Nothing -> Left Track_length_mismatch ---
      Just maybe_len -> Right (fromMaybe 0 maybe_len) ---
  transform_event :: Event_fraction note_type -> Event' (Notate_notes note_type) ---
  transform_event (Event' notes len) = Event' (transform_notes notes) len ---
  transform_notes :: Set (Note note_type) -> Notate_notes note_type ---
  transform_notes notes = ---
    case Set.elems notes of ---
      [] -> Notate_rest ---
      _ -> Notate_notes notes ---