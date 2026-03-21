{-|
Basic musical data structures: notes, lengths and events.
-}
module Composition.Notes (
  Note_type (..),
  Accidental (..),
  Basic_length (..),
  Branch (..),
  Dot (..),
  Event (..),
  Event' (..),
  Event_fraction,
  Length (..),
  Length_fraction,
  Natural_note_name (..),
  Note (..),
  Note_name (..),
  Notes (..),
  Octave,
  Pitched_or_unpitched (..),
  accidentals,
  basic_length_denominator,
  basic_length_to_fraction,
  construct_note_name,
  deconstruct_note_name,
  denominator_to_basic_length,
  events_length,
  length_to_fraction,
  lg,
  next_basic_length,
  pitched_and_unpitched,
  pitched_or_unpitched,
  scale_lengths) where
  import Control.Lens.Combinators ---
  import Data.Functor.Barbie ---
  import Data.Generics.Labels () ---
  import Data.Maybe ---
  import Data.Ratio ---
  import Data.Set
  import Data.Tuple ---
  import GHC.Generics ---
  import Parser.Utilities ---
  import Text.Read
  -- | Notes can be either pitched or unpitched.
  type data Note_type = Pitched | Unpitched
  -- | Accidentals. Mostly for internal use. ---
  data Accidental = Flat | Natural | Sharp ---
  -- | Basic note lengths.
  data Basic_length = Whole | Half | Quarter | Eighth | Sixteenth | Thirty_second | Sixty_fourth | One_hundred_and_twenty_eighth
  -- | A term-level representation of the pitched / unpitched type for branching over the type. Mostly for internal use. ---
  data Branch note_type where ---
    Branch_pitched :: Branch Pitched ---
    Branch_unpitched :: Branch Unpitched ---
  -- | Dots. ---
  data Dot = Dot ---
  -- | Events. ---
  data Event note_type = Event (Notes note_type) Length | Triplet [Event note_type] ---
  -- | Generalised events with arbitrary notes type stripped of note length display information. Mostly for internal use.
  data Event' notes = Event' {event_notes :: notes, event_length :: Length_fraction}
  -- | Events stripped of note length display information. Mostly for internal use. ---
  type Event_fraction note_type = Event' (Set (Note note_type)) ---
  -- | Note lengths. ---
  data Length = Length Basic_length [Dot] ---
  -- | Note length as a fraction. Mostly for internal use. ---
  type Length_fraction = Ratio Int ---
  -- | Natural note names. Mostly for internal use. ---
  data Natural_note_name = C_natural | D_natural | E_natural | F_natural | G_natural | A_natural | B_natural ---
  -- | Pitched and unpitched notes. ---
  data Note note_type where ---
    Pitched_note :: Octave -> Note_name -> Note Pitched ---
    Unpitched_note :: Note Unpitched ---
  -- | Note names. ---
  data Note_name = ---
    C | C_sharp | D_flat | D | D_sharp | E_flat | E | F | F_sharp | G_flat | G | G_sharp | A_flat | A | A_sharp | B_flat | B ---
  -- | Play a new set of notes or continue the previous set of notes. ---
  data Notes note_type = Notes (Set (Note note_type)) | Tie ---
  -- | Octaves in scientific pitch notation. ---
  type Octave = Int ---
  -- | This data type allows you to, for example, mix pitched and unpitched tracks in one list. ---
  data Pitched_or_unpitched f = Pitched (f Pitched) | Unpitched (f Unpitched) ---
  deriving instance Eq Accidental ---
  deriving instance Bounded Accidental
  deriving instance Bounded Natural_note_name ---
  deriving instance Bounded Note_name ---
  deriving instance Enum Accidental
  deriving instance Enum Basic_length
  deriving instance Enum Natural_note_name ---
  instance Enum (Note Pitched) where ---
    fromEnum (Pitched_note octave note_name) = 17 * octave + fromEnum note_name ---
    toEnum i = Pitched_note (div i 17) (toEnum (mod i 17)) ---
  deriving instance Enum Note_name ---
  deriving instance Generic (Event' notes) ---
  deriving instance Eq Basic_length ---
  deriving instance Eq Natural_note_name ---
  deriving instance Eq (Note note_type) ---
  deriving instance Eq Note_name ---
  instance FunctorB Pitched_or_unpitched where ---
    bmap f = pitched_or_unpitched (Pitched <$> f) (Unpitched <$> f) ---
  deriving instance Ord Accidental
  deriving instance Ord Basic_length
  deriving instance Ord Natural_note_name ---
  deriving instance Ord (Note note_type) ---
  deriving instance Ord Note_name ---
  deriving instance Read Natural_note_name
  deriving instance Read Note_name
  deriving instance Show Accidental ---
  deriving instance Show Basic_length ---
  deriving instance Show (Branch note_type) ---
  deriving instance Show Dot ---
  deriving instance Show (Event note_type) ---
  deriving instance (Show notes) => Show (Event' notes)
  deriving instance Show Length ---
  deriving instance Show Natural_note_name ---
  deriving instance Show (Note note_type) ---
  deriving instance Show Note_name ---
  deriving instance Show (Notes note_type) ---
  deriving instance (forall note_type . Show (f note_type)) => Show (Pitched_or_unpitched f) ---
  instance TraversableB Pitched_or_unpitched where ---
    btraverse f = pitched_or_unpitched ((<$>) Pitched <$> f) ((<$>) Unpitched <$> f) ---
  -- | String representations of accidentals. Mostly for internal use.
  accidentals :: [(Accidental, String)]
  accidentals = [(Flat, "b"), (Natural, ""), (Sharp, "#")]
  accidentals' :: [(Accidental, String)]
  accidentals' = [(Flat, "_flat"), (Natural, ""), (Sharp, "_sharp")]
  -- | Convert basic length to fraction denominator. Mostly for internal use. ---
  basic_length_denominator :: Basic_length -> Int ---
  basic_length_denominator len = 2 ^ basic_length_to_lg_denominator len ---
  -- | Convert basic length to fraction. Mostly for internal use. ---
  basic_length_to_fraction :: Basic_length -> Length_fraction ---
  basic_length_to_fraction len = 1 % basic_length_denominator len ---
  basic_length_to_lg_denominator :: Basic_length -> Int ---
  basic_length_to_lg_denominator len = fromJust (lookup len basic_lengths) ---
  basic_lengths :: [(Basic_length, Int)] ---
  basic_lengths = ---
    [ ---
      (Whole, 0), ---
      (Half, 1), ---
      (Quarter, 2), ---
      (Eighth, 3), ---
      (Sixteenth, 4), ---
      (Thirty_second, 5), ---
      (Sixty_fourth, 6), ---
      (One_hundred_and_twenty_eighth, 7)] ---
  -- | Construct the note name from a natural note name and an accidental. Mostly for internal use.
  construct_note_name :: Natural_note_name -> Accidental -> Maybe Note_name
  construct_note_name natural_note_name accidental =
    readMaybe (head (show natural_note_name) : fromJust (lookup accidental accidentals'))
  -- | Deconstruct a note name into the natural note name and the accidental. Mostly for internal use.
  deconstruct_note_name :: Note_name -> (Natural_note_name, Accidental)
  deconstruct_note_name note_name =
    case show note_name of
      "" -> undefined
      natural_note_name : accidental ->
        (read (natural_note_name : "_natural"), fromJust (lookup accidental (swap <$> accidentals')))
  -- | Convert fraction denominator to basic length. Mostly for internal use. ---
  denominator_to_basic_length :: Int -> Maybe Basic_length ---
  denominator_to_basic_length i = lg i >>= lg_denominator_to_basic_length ---
  -- | Events length. ---
  events_length :: [Event' notes] -> Length_fraction ---
  events_length events = sum (view #event_length <$> events) ---
  -- | Convert length to fraction. Mostly for internal use. ---
  length_to_fraction :: Length -> Length_fraction ---
  length_to_fraction (Length len dots) = basic_length_to_fraction len * (2 - 1 % (1 + length dots)) ---
  -- | Binary logarithm. Mostly for internal use. ---
  lg :: Int -> Maybe Int ---
  lg i = ---
    do ---
      check () (i > 0) ---
      lg' i ---
  lg' :: Int -> Maybe Int ---
  lg' i = ---
    case i of ---
      1 -> Just 0 ---
      _ -> ---
        case mod i 2 of ---
          0 -> (+) 1 <$> lg' (div i 2) ---
          1 -> Nothing ---
          _ -> undefined ---
  lg_denominator_to_basic_length :: Int -> Maybe Basic_length ---
  lg_denominator_to_basic_length i = lookup i (swap <$> basic_lengths) ---
  -- | Divide basic note length by two. ---
  next_basic_length :: Basic_length -> Maybe Basic_length ---
  next_basic_length len = lg_denominator_to_basic_length (1 + basic_length_to_lg_denominator len) ---
  -- | Perform an operation that doesn't depend on whether the underlying structure contains pitched or unpitched notes. ---
  pitched_and_unpitched :: (forall note_type . f note_type -> t) -> Pitched_or_unpitched f -> t ---
  pitched_and_unpitched f = pitched_or_unpitched f f ---
  -- | Perform an operation that behaves differently when the underlying structure contains pitched or unpitched notes.
  pitched_or_unpitched :: (f Pitched -> t) -> (f Unpitched -> t) -> Pitched_or_unpitched f -> t
  pitched_or_unpitched f_pitched f_unpitched x =
    case x of
      Pitched x' -> f_pitched x'
      Unpitched x' -> f_unpitched x'
  -- | Scale the length of events. Mostly for internal use. ---
  scale_lengths :: Ratio Int -> [Event' notes] -> [Event' notes] ---
  scale_lengths x events = over #event_length ((*) x) <$> events ---