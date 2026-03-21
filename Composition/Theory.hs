{-|
Basic music theory.
-}
module Composition.Theory (
  Interval (..),
  Interval_name (..),
  Semitones,
  Steps,
  distance_in_semitones,
  distance_in_steps,
  find_interval,
  interval_to_semitones,
  interval_to_steps,
  intervals_enharmonic,
  invert_interval_name,
  notes_enharmonic,
  semitones_from_c) where
  import Composition.Notes
  import Data.Maybe
  import Data.Tuple
  -- | Intervals.
  data Interval = Interval Octave Interval_name
  -- | Interval names.
  data Interval_name =
    Twice_diminished_prime |
    Diminished_prime |
    Perfect_prime |
    Diminished_second |
    Augmented_prime |
    Minor_second |
    Twice_augmented_prime |
    Major_second |
    Diminished_third |
    Augmented_second |
    Minor_third |
    Twice_diminished_fourth |
    Twice_augmented_second |
    Major_third |
    Diminished_fourth |
    Augmented_third |
    Perfect_fourth |
    Twice_diminished_fifth |
    Augmented_fourth |
    Diminished_fifth |
    Twice_augmented_fourth |
    Perfect_fifth |
    Diminished_sixth |
    Augmented_fifth |
    Minor_sixth |
    Twice_diminished_seventh |
    Twice_augmented_fifth |
    Major_sixth |
    Diminished_seventh |
    Augmented_sixth |
    Minor_seventh |
    Major_seventh |
    Augmented_seventh
  -- | Distance in semitones.
  type Semitones = Int
  -- | Distance in steps.
  type Steps = Int
  deriving instance Eq Interval
  deriving instance Eq Interval_name
  deriving instance Show Interval
  deriving instance Show Interval_name
  accidental_to_semitones :: Accidental -> Semitones
  accidental_to_semitones accidental =
    case accidental of
      Flat -> -1
      Natural -> 0
      Sharp -> 1
  construct_interval_name :: Semitones -> Steps -> Interval_name
  construct_interval_name semitones steps = fromJust (lookup (semitones, steps) (swap <$> interval_names))
  deconstruct_interval_name :: Interval_name -> (Semitones, Steps)
  deconstruct_interval_name interval_name = fromJust (lookup interval_name interval_names)
  -- | The distance in semitones between two notes.
  distance_in_semitones :: Note Pitched -> Note Pitched -> Semitones
  distance_in_semitones note_0 note_1 = interval_to_semitones (find_interval note_0 note_1)
  -- | The distance in steps between two notes.
  distance_in_steps :: Note Pitched -> Note Pitched -> Steps
  distance_in_steps note_0 note_1 = interval_to_steps (find_interval note_0 note_1)
  -- | Note that this function assumes that the inputs are ordered. If the inputs are not ordered the function will return the
  -- complement interval with a negative octave number.
  find_interval :: Note Pitched -> Note Pitched -> Interval
  find_interval (Pitched_note octave_0 note_name_0) (Pitched_note octave_1 note_name_1) =
    Interval
      (
        octave_1 -
        octave_0 -
        case compare (steps_from_c note_name_0) (steps_from_c note_name_1) of
          (LT; EQ) -> 0
          GT -> 1)
      (find_interval_name note_name_0 note_name_1)
  find_interval_name :: Note_name -> Note_name -> Interval_name
  find_interval_name note_name_0 note_name_1 =
    case compare note_name_0 note_name_1 of
      LT -> find_interval_name' note_name_0 note_name_1
      EQ -> Perfect_prime
      GT -> invert_interval_name (find_interval_name' note_name_1 note_name_0)
  find_interval_name' :: Note_name -> Note_name -> Interval_name
  find_interval_name' note_name_0 note_name_1 =
    construct_interval_name
      (semitones_from_c note_name_1 - semitones_from_c note_name_0)
      (steps_from_c note_name_1 - steps_from_c note_name_0)
  interval_name_to_semitones :: Interval_name -> Semitones
  interval_name_to_semitones interval_name = fst (deconstruct_interval_name interval_name)
  interval_name_to_steps :: Interval_name -> Steps
  interval_name_to_steps interval_name = snd (deconstruct_interval_name interval_name)
  interval_names :: [(Interval_name, (Semitones, Steps))]
  interval_names =
    [
      (Twice_diminished_prime, (-2, 0)),
      (Diminished_prime, (-1, 0)),
      (Perfect_prime, (0, 0)),
      (Diminished_second, (0, 1)),
      (Augmented_prime, (1, 0)),
      (Minor_second, (1, 1)),
      (Twice_augmented_prime, (2, 0)),
      (Major_second, (2, 1)),
      (Diminished_third, (2, 2)),
      (Augmented_second, (3, 1)),
      (Minor_third, (3, 2)),
      (Twice_diminished_fourth, (3, 3)),
      (Twice_augmented_second, (4, 1)),
      (Major_third, (4, 2)),
      (Diminished_fourth, (4, 3)),
      (Augmented_third, (5, 2)),
      (Perfect_fourth, (5, 3)),
      (Twice_diminished_fifth, (5, 4)),
      (Augmented_fourth, (6, 3)),
      (Diminished_fifth, (6, 4)),
      (Twice_augmented_fourth, (7, 3)),
      (Perfect_fifth, (7, 4)),
      (Diminished_sixth, (7, 5)),
      (Augmented_fifth, (8, 4)),
      (Minor_sixth, (8, 5)),
      (Twice_diminished_seventh, (8, 6)),
      (Twice_augmented_fifth, (9, 4)),
      (Major_sixth, (9, 5)),
      (Diminished_seventh, (9, 6)),
      (Augmented_sixth, (10, 5)),
      (Minor_seventh, (10, 6)),
      (Major_seventh, (11, 6)),
      (Augmented_seventh, (12, 6))]
  -- | The size of an interval in semitones.
  interval_to_semitones :: Interval -> Semitones
  interval_to_semitones (Interval octave interval_name) = 12 * octave + interval_name_to_semitones interval_name
  -- | The size of an interval in steps.
  interval_to_steps :: Interval -> Steps
  interval_to_steps (Interval octave interval_name) = 7 * octave + interval_name_to_steps interval_name
  -- | Checks whether two intervals are enharmonic.
  intervals_enharmonic :: Interval -> Interval -> Bool
  intervals_enharmonic interval_0 interval_1 = interval_to_semitones interval_0 == interval_to_semitones interval_1
  -- | Invert interval name.
  invert_interval_name :: Interval_name -> Interval_name
  invert_interval_name interval_name =
    let
      (semitones, steps) = deconstruct_interval_name interval_name in
      construct_interval_name (invert_interval_semitones steps semitones) (invert_interval_steps steps)
  invert_interval_semitones :: Steps -> Semitones -> Semitones
  invert_interval_semitones steps semitones =
    case steps of
      0 -> negate semitones
      _ -> 12 - semitones
  invert_interval_steps :: Steps -> Steps
  invert_interval_steps steps = mod (negate steps) 7
  -- | Checks whether two notes are enharmonic.
  notes_enharmonic :: Note Pitched -> Note Pitched -> Bool
  notes_enharmonic note_0 note_1 = 0 == distance_in_semitones note_0 note_1
  -- | Distance from C in semitones.
  semitones_from_c :: Note_name -> Semitones
  semitones_from_c note_name =
    let
      (natural_note_name, accidental) = deconstruct_note_name note_name in
      semitones_from_c_natural natural_note_name + accidental_to_semitones accidental
  semitones_from_c_natural :: Natural_note_name -> Semitones
  semitones_from_c_natural natural_note_name =
    case natural_note_name of
      C_natural -> 0
      D_natural -> 2
      E_natural -> 4
      F_natural -> 5
      G_natural -> 7
      A_natural -> 9
      B_natural -> 11
  steps_from_c :: Note_name -> Steps
  steps_from_c note_name = fromEnum (fst (deconstruct_note_name note_name))