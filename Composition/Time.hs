{-| ---
Description: Rhythmic organisation. ---
 ---
* Time signatures ---
* Note positions ---
* Measure subdivisions ---
-} ---
module Composition.Time ( ---
  Initial_position (..), ---
  Position, ---
  Time (..), ---
  Time_numerator_factor (..), ---
  initial_position_to_fraction, ---
  int_to_time_numerator_factor,
  measure_length, ---
  time_numerator_factor_to_int, ---
  time_numerator_to_int, ---
  time_subdivision) where ---
  import Composition.Errors
  import Composition.Notes
  import Data.Maybe
  import Data.Ratio
  import Data.Tuple
  -- | The timing of the first event relative to the start of the measure. For example, if the piece is in 3/4 and starts with a ---
  -- 1/4 pickup bar, the initial position can be written as @Initial_position 1 Half@ or @Initial_position 2 Quarter@. If the ---
  -- piece starts with a full bar, the numerator is 0 (and you can use any length as the denominator). ---
  data Initial_position = Initial_position Int Basic_length ---
  -- | The position of an event relative to the start of the measure. Mostly for internal use. ---
  type Position = Ratio Int ---
  -- | Time signature numerators are written as a list of factors. For example, 2 is written as @[Two]@, 3 as @[Three]@, 4 as ---
  -- @[Two Two]@, 6 as @[Two Three]@, 8 as @[Two Two Two]@, 9 as @[Three Three]@, 12 as @[Two Two Three]@, and so on. Note that ---
  -- in some applications the order matters. For example, 18 as @[Three Two Three]@ means that the bar is divided into three ---
  -- parts, each part is divided in half and then each half is divided into three parts. But 18 as @[Two Three Three]@ means ---
  -- that the bar is divided in half, each half is divided into three parts and then each part is divided into three parts. This ---
  -- will affect the placement of ties in keyboard transcriptions. ---
  data Time_numerator_factor = Two | Three ---
  -- | Time signatures. For example, 2/2 is written as @Time [Two] Half@, 3/2 as @Time [Three] Half@, 2/4 as @Time [Two] ---
  -- Quarter@, and so on. ---
  data Time = Time [Time_numerator_factor] Basic_length ---
  deriving instance Eq Time_numerator_factor
  deriving instance Show Initial_position
  deriving instance Show Time
  deriving instance Show Time_numerator_factor
  -- | Convert initial position to fraction. Mostly for internal use. ---
  initial_position_to_fraction :: Initial_position -> Position ---
  initial_position_to_fraction (Initial_position num den) = fromIntegral num * basic_length_to_fraction den ---
  -- | Convert int to time numerator factor. Mostly for internal use.
  int_to_time_numerator_factor :: Int -> Maybe Time_numerator_factor
  int_to_time_numerator_factor time_numerator_factor = lookup time_numerator_factor (swap <$> time_numerator_factors)
  -- | The length of one measure. ---
  measure_length :: Time -> Length_fraction ---
  measure_length (Time num den) = fromIntegral (time_numerator_to_int num) * basic_length_to_fraction den ---
  -- | Convert time numerator factor to int. Mostly for internal use.
  time_numerator_factor_to_int :: Time_numerator_factor -> Int
  time_numerator_factor_to_int time_numerator_factor = fromJust (lookup time_numerator_factor time_numerator_factors)
  time_numerator_factors :: [(Time_numerator_factor, Int)]
  time_numerator_factors = [(Two, 2), (Three, 3)]
  -- | Convert time numerator to int. Mostly for internal use. ---
  time_numerator_to_int :: [Time_numerator_factor] -> Int ---
  time_numerator_to_int num = product (time_numerator_factor_to_int <$> num) ---
  -- | Remove the topmost division from time signature. For example, 4/4 gets transformed into 2/4. 1/4 gets transformed into
  -- 1/8. Mostly for internal use.
  time_subdivision :: Time -> Either Error Time
  time_subdivision (Time num den) =
    case num of
      [] ->
        case next_basic_length den of
          Nothing -> Left (Is_out_of_range_without_location Note_length_denominator_IOOR)
          Just den' -> Right (Time [] den')
      _ : num' -> Right (Time num' den)