{-|
Scores that can be converted to Lilypond and MIDI files.
-}
module Composition.Score (
  Clef (..),
  Clef_name (..),
  Header_field (..),
  Initial_position (..),
  Instrument_and_staves (..),
  Key,
  MIDI_instrument,
  Part (..),
  Score (..),
  Stave (..),
  Tempo,
  Time (..),
  Time_numerator_factor (..),
  Tracks (..),
  Velocity,
  int_to_time_numerator_factor,
  max_velocity,
  time_numerator_factor_to_int,
  time_numerator_to_int) where
  import Composition.Notes
  import Composition.Theory
  import Data.List as List
  import Data.Map.Strict
  import Data.Maybe
  import Data.Set as Set
  import Data.Tuple
  import Data.Word
  -- | Clefs (with transposition).
  data Clef note_type where
    Pitched_clef :: {clef_name :: Clef_name, transposition :: Steps} -> Clef Pitched
    Percussion_clef :: Clef Unpitched
  -- | Clef names.
  data Clef_name = Subbass | Bass | Baritone_F | Baritone_C | Tenor | Alto | Mezzosoprano | Soprano | Treble | French
  -- | Score header fields.
  data Header_field = Left_text | Overtitle | Right_text | Subtitle | Subsubtitle | Title
  -- | The timing of the first event relative to the start of the measure. For example, if the piece is in 3/4 and starts with a
  -- 1/4 partial bar the initial position can be written as @Initial_position 1 Half@ or @Initial_position 2 Quarter@. If the
  -- piece starts with a full bar the numerator is 0 (and you can use any length as the denominator).
  data Initial_position = Initial_position Int Basic_length
  -- | Group of staves for one instrument.
  data Instrument_and_staves note_type =
    Instrument_and_staves {
      instrument_name :: String,
      short_instrument_name :: String,
      midi_instrument :: MIDI_instrument,
      velocity :: Velocity,
      staves :: [Stave note_type]}
  -- | A set of note names that serves as a key signature. For example, the key signature of C minor would be @[E_flat, A_flat,
  -- B_flat]@. Conflicting accidentals will result in an error.
  type Key = Set Note_name
  -- | MIDI instrument code.
  type MIDI_instrument = Word8
  -- | Each part can have a different key, time signature and instrumentation.
  data Part =
    Part {
      title :: String,
      key :: Key,
      time :: Time,
      initial_position :: Initial_position,
      tempo :: Tempo,
      stave_groups :: [[Pitched_or_unpitched Instrument_and_staves]]}
  -- | Score that can be converted to Lilypond and MIDI files.
  data Score = Score {header :: Map Header_field String, parts :: [Part]}
  -- | Stave with clef.
  data Stave note_type = Stave {clef :: Clef note_type, tracks :: Tracks note_type}
  -- | Tempo in beats per minute.
  type Tempo = Int
  -- | Time signature numerators are written as a list of factors. For example, 2 is written as @[Two]@, 3 as @[Three]@, 4 as
  -- @[Two Two]@, 6 as @[Two Three]@, 8 as @[Two Two Two]@, 9 as @[Three Three]@, 12 as @[Two Two Three]@, and so on. Note that
  -- in some applications the order matters. For example, 18 as @[Three Two Three]@ means that the bar is divided into three
  -- parts, each part is divided in half and then each half is divided into three parts. But 18 as @[Two Three Three]@ means
  -- that the bar is divided in half, each half is divided into three parts and then each part is divided into three parts. This
  -- will affect the placement of ties in keyboard transcriptions.
  data Time_numerator_factor = Two | Three
  -- | Time signatures. For example, 2/2 is written as @Time [Two] Half@, 3/2 as @Time [Three] Half@, 2/4 as @Time [Two]
  -- Quarter@, and so on.
  data Time = Time [Time_numerator_factor] Basic_length
  -- | One stave can fit up to two rhythmically independent tracks.
  data Tracks note_type = One_track [Event note_type] | Two_tracks [Event note_type] [Event note_type]
  -- | MIDI volume ranges from 0 to 127.
  type Velocity = Word8
  deriving instance Eq Clef_name
  deriving instance Eq Header_field
  deriving instance Eq Time_numerator_factor
  deriving instance Ord Clef_name
  deriving instance Ord Header_field
  deriving instance Read Clef_name
  deriving instance Read Header_field
  deriving instance Show (Clef note_type)
  deriving instance Show Clef_name
  deriving instance Show Header_field
  deriving instance Show Initial_position
  deriving instance Show (Instrument_and_staves note_type)
  deriving instance Show Part
  deriving instance Show Score
  deriving instance Show (Stave note_type)
  deriving instance Show Time
  deriving instance Show Time_numerator_factor
  deriving instance Show (Tracks note_type)
  -- | Convert int to time numerator factor. Mostly for internal use.
  int_to_time_numerator_factor :: Int -> Maybe Time_numerator_factor
  int_to_time_numerator_factor time_numerator_factor = List.lookup time_numerator_factor (swap <$> time_numerator_factors)
  -- | Maximum MIDI volume.
  max_velocity :: Velocity
  max_velocity = 127
  -- | Convert time numerator factor to int. Mostly for internal use.
  time_numerator_factor_to_int :: Time_numerator_factor -> Int
  time_numerator_factor_to_int time_numerator_factor = fromJust (List.lookup time_numerator_factor time_numerator_factors)
  time_numerator_factors :: [(Time_numerator_factor, Int)]
  time_numerator_factors = [(Two, 2), (Three, 3)]
  -- | Convert time numerator to int. Mostly for internal use.
  time_numerator_to_int :: [Time_numerator_factor] -> Int
  time_numerator_to_int num = product (time_numerator_factor_to_int <$> num)