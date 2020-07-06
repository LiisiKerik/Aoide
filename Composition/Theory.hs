--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: A module for basic music theory.

This module contains functions for computing intervals and classifying chords.
-}
module Composition.Theory (
  Interval_name (..),
  Interval_number (..),
  Interval_quality (..),
  accidental_semitones,
  compute_interval_name,
  invert_interval_name,
  semitones_from_c,
  steps_from_c) where
  import Composition.Notes (Accidental (..), Natural_note_name (..), Note_name, Note_name' (..), deconstruct_note_name)
  -- | Interval name.
  data Interval_name = Interval_name Interval_quality Interval_number
  -- | Interval numbers.
  data Interval_number = Prime | Second | Third | Fourth | Fifth | Sixth | Seventh
  -- | Interval qualities, allowing for up to three levels of augmentation.
  data Interval_quality =
    Thrice_diminished | Twice_diminished | Diminished | Minor | Perfect | Major | Augmented | Twice_augmented | Thrice_augmented
  deriving instance Enum Interval_number
  deriving instance Eq Interval_number
  deriving instance Ord Interval_number
  deriving instance Show Interval_name
  deriving instance Show Interval_number
  deriving instance Show Interval_quality
  -- | Returns the number of semitones by which the accidental adjusts the pitch.
  accidental_semitones :: Accidental -> Int
  accidental_semitones accidental =
    case accidental of
      Flat -> -1
      Natural -> 0
      Sharp -> 1
  -- | Computes the interval between two note names.
  compute_interval_name :: Note_name -> Note_name -> Interval_name
  compute_interval_name note_name_0 note_name_1 =
    case compare note_name_0 note_name_1 of
      LT -> compute_interval_name' note_name_0 note_name_1
      EQ -> Interval_name Perfect Prime
      GT -> invert_interval_name (compute_interval_name' note_name_1 note_name_0)
  compute_interval_name' :: Note_name -> Note_name -> Interval_name
  compute_interval_name' note_name_0 note_name_1 =
    let
      interval_number = toEnum (mod (steps_from_c note_name_1 - steps_from_c note_name_0) 7)
    in
      Interval_name
        (compute_interval_quality interval_number (semitones_from_c note_name_1 - semitones_from_c note_name_0))
        interval_number
  compute_interval_quality :: Interval_number -> Int -> Interval_quality
  compute_interval_quality interval_number =
    case interval_number of
      Prime -> compute_perfect_interval_quality 0
      Second -> compute_minor_or_major_interval_quality 1
      Third -> compute_minor_or_major_interval_quality 3
      Fourth -> compute_perfect_interval_quality 5
      Fifth -> compute_perfect_interval_quality 7
      Sixth -> compute_minor_or_major_interval_quality 8
      Seventh -> compute_minor_or_major_interval_quality 10
  compute_minor_or_major_interval_quality :: Int -> Int -> Interval_quality
  compute_minor_or_major_interval_quality minor_semitones semitones =
    case mod (semitones - minor_semitones) 12 of
      0 -> Minor
      1 -> Major
      2 -> Augmented
      3 -> Twice_augmented
      4 -> Thrice_augmented
      9 -> Thrice_diminished
      10 -> Twice_diminished
      11 -> Diminished
      _ -> undefined
  compute_perfect_interval_quality :: Int -> Int -> Interval_quality
  compute_perfect_interval_quality perfect_semitones semitones =
    case semitones - perfect_semitones of
      -3 -> Thrice_diminished
      -2 -> Twice_diminished
      -1 -> Diminished
      0 -> Perfect
      1 -> Augmented
      2 -> Twice_augmented
      3 -> Thrice_augmented
      _ -> undefined
  -- | Inverts the interval name.
  invert_interval_name :: Interval_name -> Interval_name
  invert_interval_name (Interval_name quality interval_number) =
    Interval_name (invert_interval_quality quality) (invert_interval_number interval_number)
  invert_interval_quality :: Interval_quality -> Interval_quality
  invert_interval_quality quality =
    case quality of
      Thrice_diminished -> Thrice_augmented
      Twice_diminished -> Twice_augmented
      Diminished -> Augmented
      Minor -> Major
      Perfect -> Perfect
      Major -> Minor
      Augmented -> Diminished
      Twice_augmented -> Twice_diminished
      Thrice_augmented -> Thrice_diminished
  invert_interval_number :: Interval_number -> Interval_number
  invert_interval_number interval_number = toEnum (mod (negate (fromEnum interval_number)) 7)
  -- | Computes the number of semitones from C. C♭ is considered to be -1 semitone from C and B♯ 12 semitones from C.
  semitones_from_c :: Note_name -> Int
  semitones_from_c note_name =
    let
      Note_name' natural_note_name accidental = deconstruct_note_name note_name
    in
      semitones_from_c_natural natural_note_name + accidental_semitones accidental
  semitones_from_c_natural :: Natural_note_name -> Int
  semitones_from_c_natural natural_note_name =
    case natural_note_name of
      C_natural -> 0
      D_natural -> 2
      E_natural -> 4
      F_natural -> 5
      G_natural -> 7
      A_natural -> 9
      B_natural -> 11
  -- | Computes the number of steps from C.
  steps_from_c :: Note_name -> Int
  steps_from_c note_name =
    let
      Note_name' natural_note_name _ = deconstruct_note_name note_name
    in
      fromEnum natural_note_name
--------------------------------------------------------------------------------------------------------------------------------