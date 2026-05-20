module Composition.Notation (
  Event' (..),
  Event_fraction,
  Length_fraction,
  Position,
  Track (..),
  add_notational_information,
  events_length,
  initial_position_to_fraction,
  length_to_fraction,
  measure_length,
  remove_notational_information,
  scale_lengths,
  tracks_length,
  tracks_to_list) where
  import Composition.Errors
  import Composition.Notes
  import Composition.Score
  import Control.Lens.Combinators
  import Control.Monad
  import Control.Monad.Except
  import Data.Fixed
  import Data.Foldable
  import Data.Functor.Barbie
  import Data.Generics.Labels ()
  import Data.Maybe
  import Data.Ratio
  import Data.Set
  import GHC.Generics
  import GHC.Real
  import Parser.Utilities
  data Add_notational_information_notes note_type =
    Add_notational_information_rest |
    Add_notational_information_notes (Set (Note note_type)) |
    Add_notational_information_tie
  -- | Generalised events with arbitrary notes type stripped of note length display information. Mostly for internal use.
  data Event' notes = Event' {event_notes :: notes, event_length :: Length_fraction}
  -- | Events stripped of note length display information. Mostly for internal use.
  type Event_fraction note_type = Event' (Set (Note note_type))
  -- | Note length as a fraction. Mostly for internal use.
  type Length_fraction = Ratio Int
  -- | The position of an event relative to the start of the measure. Mostly for internal use.
  type Position = Ratio Int
  -- | Tracks stripped of notational information (brackets, instrument names, clefs and note length display information). Mostly
  -- for internal use.
  data Track note_type = Track {instrument :: MIDI_instrument, velocity :: Velocity, events :: [Event_fraction note_type]}
  deriving instance Generic (Event' notes)
  deriving instance Show (Add_notational_information_notes note_type)
  deriving instance (Show notes) => Show (Event' notes)
  deriving instance Show (Track note_type)
  -- | Add note length display information to events. Mostly for internal use.
  add_notational_information :: Time -> Initial_position -> [Event_fraction note_type] -> Either Error [Event note_type]
  add_notational_information time initial_position events =
    do
      traverse_ check_event_length events
      initial_position' <- initial_position_to_fraction time initial_position
      add_notational_information_events time initial_position' (transform_event <$> events)
  add_notational_information_basic_length :: Length_fraction -> Maybe Basic_length
  add_notational_information_basic_length (num :% den) =
    case num of
      1 -> denominator_to_basic_length den
      _ -> Nothing
  add_notational_information_events :: forall note_type .
    Time -> Position -> [Event' (Add_notational_information_notes note_type)] -> Either Error [Event note_type]
  add_notational_information_events time position events =
    join <$> traverse (add_notational_information_measure time) separate_measures where
    separate_measures :: [(Length_fraction, [Event' (Add_notational_information_notes note_type)])]
    separate_measures =
      case separate_measures' position events of
        [] -> []
        measure : measures' -> (position, measure) : ((,) 0 <$> measures')
    separate_measures' ::
      (
        Position ->
        [Event' (Add_notational_information_notes note_type)] ->
        [[Event' (Add_notational_information_notes note_type)]])
    separate_measures' position' events' =
      case events' of
        [] -> []
        Event' notes len : events'' ->
          let
            position'' = position' + len in
            case compare position'' (measure_length time) of
              LT ->
                case separate_measures' position'' events'' of
                  [] -> [[Event' notes len]]
                  measure : measures -> (Event' notes len : measure) : measures
              EQ -> [Event' notes len] : separate_measures' 0 events''
              GT ->
                let
                  len' = measure_length time - position' in
                  [Event' notes len'] : separate_measures' 0 (Event' (notes_to_tie notes) (len - len') : events'')
  add_notational_information_measure ::
    Time -> (Position, [Event' (Add_notational_information_notes note_type)]) -> Either Error [Event note_type]
  add_notational_information_measure time (position, events) =
    do
      binary_blocks <- separate_binary_blocks position events
      join <$> traverse add_notational_information_binary_block binary_blocks where
    add_notational_information_binary_block ::
      (Position, [Event' (Add_notational_information_notes note_type)]) -> Either Error [Event note_type]
    add_notational_information_binary_block (position', events') =
      (case events' of
        [event@(Event' notes len)] ->
          case add_notational_information_simple_length len of
            Nothing -> divide_time position' [event]
            Just len' -> Right [Event (add_notational_information_notes notes) len']
        _ ->
          let
            num :% den = events_length events' in
            (
              (\ events'' -> [Triplet events'']) <$>
              case num of
                1 ->
                  case denominator_to_basic_length (2 * den) of
                    Nothing -> Left (Is_out_of_range_without_location Note_length_denominator_IOOR)
                    Just den' -> add_notational_information_measure (Time [Three] den') (0, scale_lengths (3 % 2) events')
                _ -> divide_time position' events'))
    divide_time :: Position -> [Event' (Add_notational_information_notes note_type)] -> Either Error [Event note_type]
    divide_time position' events' =
      do
        time' <- time_subdivision time
        add_notational_information_events time' (mod' position' (measure_length time')) events'
    separate_binary_blocks ::
      (
        Position ->
        [Event' (Add_notational_information_notes note_type)] ->
        Either Error [(Position, [Event' (Add_notational_information_notes note_type)])])
    separate_binary_blocks position' events' =
      case events' of
        [] -> Right []
        event@(Event' _ len) : events'' ->
          do
            let position'' = position' + len
            binary_blocks <- separate_binary_blocks position'' events''
            case is_smooth [2] position'' of
              False ->
                case binary_blocks of
                  [] -> Left Track_ends_with_an_incomplete_triplet
                  (_, events''') : binary_blocks' -> Right ((position', event : events''') : binary_blocks')
              True -> Right ((position', [event]) : binary_blocks)
  add_notational_information_notes :: Add_notational_information_notes note_type -> Notes note_type
  add_notational_information_notes notes =
    case notes of
      Add_notational_information_rest -> Notes empty
      Add_notational_information_notes notes' -> Notes notes'
      Add_notational_information_tie -> Tie
  add_notational_information_simple_length :: Length_fraction -> Maybe Length
  add_notational_information_simple_length (num :% den) =
    do
      dots <- (\ i -> i - 1) <$> lg (1 + num)
      len <- add_notational_information_basic_length (2 ^ dots % den)
      Just (Length len (replicate dots Dot))
  check_event_length :: Event' notes -> Either Error ()
  check_event_length (Event' _ len) =
    do
      check Non_positive_note_length (0 < len)
      check Note_length_denominator_contains_factors_other_than_2_and_3 (is_smooth [2, 3] len)
      return ()
  -- | Events length.
  events_length :: [Event' notes] -> Length_fraction
  events_length events = sum (view #event_length <$> events)
  -- | Convert initial position to fraction. Mostly for internal use.
  initial_position_to_fraction :: (MonadError Error f) => Time -> Initial_position -> f Position
  initial_position_to_fraction time (Initial_position num den) =
    do
      let initial_position' = num % basic_length_denominator den
      check
        Initial_position_is_out_of_range
        (between 0 (measure_length time - length_to_fraction (Length maxBound [])) initial_position')
      return initial_position'
  is_smooth :: [Int] -> Ratio Int -> Bool
  is_smooth factors x = is_smooth' factors (denominator x)
  is_smooth' :: [Int] -> Int -> Bool
  is_smooth' factors i =
    case i of
      1 -> True
      _ ->
        case factors of
          [] -> False
          factor : factors' -> is_smooth' factors' (remove_factor factor i)
  join_events :: [Event' (Notes note_type)] -> [Event_fraction note_type]
  join_events events =
    case events of
      [] -> []
      Event' notes len : events' -> join_events' (Event' (notes_to_set notes) len) events'
  join_events' :: Event_fraction note_type -> [Event' (Notes note_type)] -> [Event_fraction note_type]
  join_events' (Event' notes_0 len_0) events =
    case events of
      [] -> [Event' notes_0 len_0]
      Event' maybe_notes_1 len_1 : events' ->
        case join_notes notes_0 maybe_notes_1 of
          Nothing -> join_events' (Event' notes_0 (len_0 + len_1)) events'
          Just notes_1 -> Event' notes_0 len_0 : join_events' (Event' notes_1 len_1) events'
  join_notes :: Set (Note note_type) -> Notes note_type -> Maybe (Set (Note note_type))
  join_notes notes_0 maybe_notes_1 =
    case maybe_notes_1 of
      Notes notes_1 ->
        case (elems notes_0, elems notes_1) of
          ([], []) -> Nothing
          _ -> Just notes_1
      Tie -> Nothing
  -- | Convert length to fraction. Mostly for internal use.
  length_to_fraction :: Length -> Length_fraction
  length_to_fraction (Length len dots) = (1 % basic_length_denominator len) * (2 - 1 % 2 ^ length dots)
  -- | The length of one measure. Mostly for internal use.
  measure_length :: Time -> Length_fraction
  measure_length (Time num den) = time_numerator_to_int num % basic_length_denominator den
  notes_to_set :: Notes note_type -> Set (Note note_type)
  notes_to_set maybe_notes =
    case maybe_notes of
      Notes notes -> notes
      Tie -> empty
  notes_to_tie :: Add_notational_information_notes note_type -> Add_notational_information_notes note_type
  notes_to_tie notes =
    case notes of
      Add_notational_information_rest -> Add_notational_information_rest
      Add_notational_information_notes _ -> Add_notational_information_tie
      Add_notational_information_tie -> Add_notational_information_tie
  remove_factor :: Int -> Int -> Int
  remove_factor factor i =
    case mod i factor of
      0 -> remove_factor factor (div i factor)
      _ -> i
  -- | Strip tracks of notational information (brackets, instrument names, clefs, note length display information). Mostly for
  -- internal use.
  remove_notational_information :: [[Pitched_or_unpitched Instrument_and_staves]] -> [Pitched_or_unpitched Track]
  remove_notational_information stave_groups =
    stave_groups >>= flip (>>=) (btraverse remove_notational_information_instrument_and_staves)
  remove_notational_information_stave :: Stave note_type -> [[Event_fraction note_type]]
  remove_notational_information_stave (Stave {tracks}) = remove_notational_information_events <$> tracks_to_list tracks
  remove_notational_information_events :: [Event note_type] -> [Event_fraction note_type]
  remove_notational_information_events events = join_events (remove_notational_information_events' events)
  remove_notational_information_events' :: [Event note_type] -> [Event' (Notes note_type)]
  remove_notational_information_events' events = events >>= remove_notational_information_event
  remove_notational_information_event :: Event note_type -> [Event' (Notes note_type)]
  remove_notational_information_event event =
    case event of
      Event notes len -> [Event' notes (length_to_fraction len)]
      Triplet events -> scale_lengths (2 % 3) (remove_notational_information_events' events)
  remove_notational_information_instrument_and_staves :: Instrument_and_staves note_type -> [Track note_type]
  remove_notational_information_instrument_and_staves (Instrument_and_staves {midi_instrument, velocity, staves}) =
    do
      events <- staves >>= remove_notational_information_stave
      [Track {instrument = midi_instrument, velocity, events}]
  -- | Scale the length of events. Mostly for internal use.
  scale_lengths :: Ratio Int -> [Event' notes] -> [Event' notes]
  scale_lengths x events = over #event_length ((*) x) <$> events
  time_subdivision :: Time -> Either Error Time
  time_subdivision (Time num den) =
    case num of
      [] ->
        case next_basic_length den of
          Nothing -> Left (Is_out_of_range_without_location Note_length_denominator_IOOR)
          Just den' -> Right (Time [] den')
      _ : num' -> Right (Time num' den)
  track_length :: Track note_type -> Ratio Int
  track_length (Track {events}) = events_length events
  -- | Calculate part length. Mostly for internal use.
  tracks_length :: [Pitched_or_unpitched Track] -> Either Error Length_fraction
  tracks_length tracks =
    case all_equal (pitched_and_unpitched track_length <$> tracks) of
      Nothing -> Left Track_length_mismatch
      Just maybe_len -> Right (fromMaybe 0 maybe_len)
  -- | Convert the stave to a list of voices. Mostly for internal use.
  tracks_to_list :: Tracks note_type -> [[Event note_type]]
  tracks_to_list stave =
    case stave of
      One_track events -> [events]
      Two_tracks events_0 events_1 -> [events_0, events_1]
  transform_event :: Event_fraction note_type -> Event' (Add_notational_information_notes note_type)
  transform_event (Event' notes len) = Event' (transform_notes notes) len
  transform_notes :: Set (Note note_type) -> Add_notational_information_notes note_type
  transform_notes notes =
    case elems notes of
      [] -> Add_notational_information_rest
      _ -> Add_notational_information_notes notes