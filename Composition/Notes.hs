--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveLift, StandaloneDeriving, TemplateHaskell #-}
{-|
Description: A module for basic musical data structures.

This module contains some data structures for describing note sequences and time signatures, and a QuasiQuoter for note
sequences.
-}
module Composition.Notes (
  Accidental (..),
  Natural_note_name (..),
  Note (..),
  Note_name (..),
  Note_name' (..),
  Rat,
  Simultaneous (..),
  Time (..),
  Time_and_position (..),
  construct_note_name,
  deconstruct_note_name,
  ly,
  measure_length,
  sequential_length,
  simultaneous_length,
  subdivision) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.RWS.Strict (RWS, RWST (..), runRWS)
  import Control.Monad.State.Strict (MonadState (..), modify)
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Data.Char (isDigit)
  import Data.Maybe (fromJust)
  import Data.Ratio ((%), Ratio)
  import Data.Tuple (swap)
  import Language.Haskell.TH (Exp, Q)
  import Language.Haskell.TH.Quote (QuasiQuoter (..))
  import Language.Haskell.TH.Syntax (Lift (..))
  -- | Accidentals.
  data Accidental = Flat | Natural | Sharp
  data Char' = Delimiter_char Token | Nat_char Char | Negation_char | Space_char
  data Counter = Counter Integer
  -- | Natural note names for algorithmic convenience.
  data Natural_note_name = C_natural | D_natural | E_natural | F_natural | G_natural | A_natural | B_natural
  -- | Note. The first argument is the octave.
  data Note = Note Int Note_name
  -- | Note names. Double accidentals are not supported.
  data Note_name =
    C_flat |
    C |
    C_sharp |
    D_flat |
    D |
    D_sharp |
    E_flat |
    E |
    F_flat |
    E_sharp |
    F |
    F_sharp |
    G_flat |
    G |
    G_sharp |
    A_flat |
    A |
    A_sharp |
    B_flat |
    B |
    B_sharp
  -- | An alternative representation of note names that is more convenient for algorithms.
  data Note_name' =
    Note_name' Natural_note_name Accidental
  type Parser = WST Counter [Token] Maybe
  -- | Rationals with limited numerators and denominators for representing note lengths.
  type Rat = Ratio Int
  -- | A collection of notes of same length that sound simultaneously. Non-positive length will result in runtime errors when
  -- attempting to generate Lilypond and MIDI files.
  data Simultaneous = Simultaneous [Note] Rat
  -- | Time signature. The first argument describes the subdivisions of the bar. For example, 2 beats per bar is encoded as [2],
  -- 3 as [3], 4 as [2, 2], 6 as [2, 3], 9 as [3, 3]. The second argument is the inverse of the length of the beat. Note that
  -- non-positive numbers in either numerator or denominator will result in errors, and Lilypond does not accept denominators
  -- that are not a power of two.
  data Time = Time [Int] Int
  -- | Time signature and the starting position of the first bar. For example, if the piece starts with a full bar, the initial
  -- position is 0. If the piece is in 3/4 and starts with a 1/4-note bar, the initial position is 1/2.
  data Time_and_position = Time_and_position Time Rat
  data Token =
    Dot_token |
    End_token |
    Flat_token |
    Left_angular_token |
    Nat_token Int |
    Negation_token |
    Note_name_token Natural_note_name |
    Right_angular_token |
    Sharp_token |
    Tie_token
  type Tokeniser = WS [Token] [Char']
  type WS output state = RWS () output state
  type WST = RWST ()
  infixl 3 <+>
  (<+>) :: Parser t -> Parser t -> Parser t
  parse_0 <+> parse_1 =
    wst
      (\tokens ->
        case (runWST parse_0 tokens, runWST parse_1 tokens) of
          (Nothing, Nothing) -> Nothing
          (Nothing, Just result) -> Just result
          (Just result, Nothing) -> Just result
          (Just result_0, Just result_1) ->
            case compare (get_token_counter result_0) (get_token_counter result_1) of
              LT -> Just result_1
              EQ -> Nothing
              GT -> Just result_0)
  deriving instance Enum Natural_note_name
  instance Enum Note where
    fromEnum (Note octave note_name) =
      (
        21 * octave +
        fromEnum note_name -
        case note_name of
          C_flat -> 2
          B_sharp -> 0
          _ -> 1)
    toEnum i =
      let
        octave = div i 21
      in
        case mod i 21 of
          19 -> Note (1 + octave) C_flat
          20 -> Note octave B_sharp
          j -> Note octave (toEnum (1 + j))
  deriving instance Enum Note_name
  deriving instance Eq Accidental
  deriving instance Eq Counter
  deriving instance Eq Natural_note_name
  deriving instance Eq Note
  deriving instance Eq Note_name
  deriving instance Eq Note_name'
  deriving instance Eq Token
  deriving instance Lift Note
  deriving instance Lift Note_name
  deriving instance Lift Simultaneous
  instance Monoid Counter where
    mempty = 0
  instance Num Counter where
    Counter i * Counter j = Counter (i * j)
    Counter i + Counter j = Counter (i + j)
    abs (Counter i) = Counter (abs i)
    fromInteger i = Counter i
    negate (Counter i) = Counter (negate i)
    signum (Counter i) = Counter (signum i)
  deriving instance Ord Counter
  instance Ord Note where
    compare (Note octave_0 note_name_0) (Note octave_1 note_name_1) =
      case compare octave_0 octave_1 of
        LT ->
          case (note_name_0, note_name_1) of
            (B_sharp, C_flat) -> GT
            _ -> LT
        EQ -> compare note_name_0 note_name_1
        GT ->
          case (note_name_0, note_name_1) of
            (C_flat, B_sharp) -> LT
            _ -> GT
  deriving instance Ord Note_name
  instance Semigroup Counter where
    (<>) = (+)
  deriving instance Show Accidental
  deriving instance Show Char'
  deriving instance Show Counter
  deriving instance Show Natural_note_name
  deriving instance Show Note
  deriving instance Show Note_name
  deriving instance Show Note_name'
  deriving instance Show Simultaneous
  deriving instance Show Time
  deriving instance Show Time_and_position
  deriving instance Show Token
  add_token :: Token -> Tokeniser ()
  add_token token = tell [token]
  certain_token :: Token -> Token -> Maybe ()
  certain_token token token' =
    case token == token' of
      False -> Nothing
      True -> Just ()
  classify_char :: Char -> Char'
  classify_char c =
    case c of
      ' ' -> Space_char
      '#' -> Delimiter_char Sharp_token
      '-' -> Negation_char
      '.' -> Delimiter_char Dot_token
      '<' -> Delimiter_char Left_angular_token
      '>' -> Delimiter_char Right_angular_token
      'A' -> Delimiter_char (Note_name_token A_natural)
      'B' -> Delimiter_char (Note_name_token B_natural)
      'C' -> Delimiter_char (Note_name_token C_natural)
      'D' -> Delimiter_char (Note_name_token D_natural)
      'E' -> Delimiter_char (Note_name_token E_natural)
      'F' -> Delimiter_char (Note_name_token F_natural)
      'G' -> Delimiter_char (Note_name_token G_natural)
      'b' -> Delimiter_char Flat_token
      '~' -> Delimiter_char Tie_token
      _ ->
        case isDigit c of
          False -> error "Invalid character."
          True -> Nat_char c
  -- | Construct the note name from a natural note name and an accidental.
  construct_note_name :: Note_name' -> Note_name
  construct_note_name note_name = fromJust (lookup note_name (swap <$> note_names))
  -- | Deconstruct a note name into the natural note name and the accidental.
  deconstruct_note_name :: Note_name -> Note_name'
  deconstruct_note_name note_name = fromJust (lookup note_name note_names)
  gather_nat :: Tokeniser String
  gather_nat =
    do
      maybe_char <- get_char 0
      case maybe_char of
        Just (Nat_char c) ->
          do
            next_char
            i <- gather_nat
            return (c : i)
        _ -> return ""
  get_char :: Integer -> Tokeniser (Maybe Char')
  get_char i = index i <$> get
  get_token_counter :: (t, [Token], Counter) -> Counter
  get_token_counter (_, _, token_counter) = token_counter
  index :: Integer -> [t] -> Maybe t
  index i x =
    case x of
      [] -> Nothing
      y : z ->
        case i of
          0 -> Just y
          _ -> index (i - 1) z
  invalid_template_location :: String -> Q t
  invalid_template_location = template_error "Invalid location for template ly."
  -- | A QuasiQuoter for compile-time parsing of note sequences. The syntax is loosely based on Lilypond. Some example of use:
  --
  -- * An empty note sequence: @[ly||]@.
  -- * A single note: @[ly|\<C0>1|]@.
  -- * Two consecutive notes: @[ly|\<C0>1 \<C0>1|]@.
  -- * A rest, a single note and two simultaneous notes: @[ly|\<>1 \<C0>1 \<C0 D0>1|]@.
  -- * All natural notes from C to B: @[ly|\<C0 D0 E0 F0 G0 A0 B0>1|]@.
  -- * Accidentals: @[ly|\<Cb0 C0 C#0>1|]@.
  -- * Different octaves: @[ly|\<C-1 C0 C1>1|]@.
  -- * Rests of length 1\/3, 1\/2, 2\/3, 1, 3\/2, 2 and 3: @[ly|\<>3 \<>2 \<>3~3 \<>1 \<>1. \<>1~1 \<>1~1~1|]@.
  ly :: QuasiQuoter
  ly = QuasiQuoter ly_exp invalid_template_location invalid_template_location invalid_template_location
  ly_exp :: String -> Q Exp
  ly_exp = parse parse_sequential
  -- | The length of one measure.
  measure_length :: Time -> Rat
  measure_length (Time numerator denominator) = product numerator % denominator
  nat_token :: Token -> Maybe Int
  nat_token token =
    case token of
      Nat_token i -> Just i
      _ -> Nothing
  next_char :: Tokeniser ()
  next_char = modify tail
  note_name_token :: Token -> Maybe Natural_note_name
  note_name_token token =
    case token of
      Note_name_token natural_note_name -> Just natural_note_name
      _ -> Nothing
  note_names :: [(Note_name, Note_name')]
  note_names =
    [
      (C_flat, Note_name' C_natural Flat),
      (C, Note_name' C_natural Natural),
      (C_sharp, Note_name' C_natural Sharp),
      (D_flat, Note_name' D_natural Flat),
      (D, Note_name' D_natural Natural),
      (D_sharp, Note_name' D_natural Sharp),
      (E_flat, Note_name' E_natural Flat),
      (E, Note_name' E_natural Natural),
      (E_sharp, Note_name' E_natural Sharp),
      (F_flat, Note_name' F_natural Flat),
      (F, Note_name' F_natural Natural),
      (F_sharp, Note_name' F_natural Sharp),
      (G_flat, Note_name' G_natural Flat),
      (G, Note_name' G_natural Natural),
      (G_sharp, Note_name' G_natural Sharp),
      (A_flat, Note_name' A_natural Flat),
      (A, Note_name' A_natural Natural),
      (A_sharp, Note_name' A_natural Sharp),
      (B_flat, Note_name' B_natural Flat),
      (B, Note_name' B_natural Natural),
      (B_sharp, Note_name' B_natural Sharp)]
  parse :: Lift t => Parser t -> String -> Q Exp
  parse parse_t text =
    let
      x = parse' parse_t (tokenise text)
    in
      [e|x|]
  parse' :: Parser t -> [Token] -> t
  parse' parse_t tokens =
    case runWST (parse_end parse_t) tokens of
      Nothing -> template_error "Parse error."
      Just (x, _, _) -> x
  parse_accidental :: Parser Accidental
  parse_accidental = parse_flat <+> parse_natural <+> parse_sharp
  parse_angular :: Parser t -> Parser t
  parse_angular = parse_brackets Left_angular_token Right_angular_token
  parse_base_length :: Parser Rat
  parse_base_length = (%) 1 <$> parse_nat
  parse_brackets :: Token -> Token -> Parser t -> Parser t
  parse_brackets left_bracket right_bracket parse_t =
    do
      parse_token left_bracket
      x <- parse_t
      parse_token right_bracket
      return x
  parse_dots :: Parser Int
  parse_dots = length <$> parse_many (parse_token Dot_token)
  parse_element :: Token -> Parser t -> Parser t
  parse_element separator parse_t =
    do
      parse_token separator
      parse_t
  parse_empty :: Parser [t]
  parse_empty = return []
  parse_end :: Parser t -> Parser t
  parse_end parse_t =
    do
      x <- parse_t
      parse_token End_token
      return x
  parse_flat :: Parser Accidental
  parse_flat =
    do
      parse_token Flat_token
      return Flat
  parse_int :: Parser Int
  parse_int = parse_negative_int <+> parse_nat
  parse_length :: Parser Rat
  parse_length = sum <$> parse_list Tie_token parse_simple_length
  parse_list :: Token -> Parser t -> Parser [t]
  parse_list separator parse_t = (:) <$> parse_t <*> parse_many (parse_element separator parse_t)
  parse_many :: Parser t -> Parser [t]
  parse_many parse_t = parse_empty <+> parse_some parse_t
  parse_nat :: Parser Int
  parse_nat = parse_token' nat_token
  parse_natural :: Parser Accidental
  parse_natural = return Natural
  parse_natural_note_name :: Parser Natural_note_name
  parse_natural_note_name = parse_token' note_name_token
  parse_negative_int :: Parser Int
  parse_negative_int =
    do
      parse_token Negation_token
      i <- parse_nat
      return (negate i)
  parse_note :: Parser Note
  parse_note =
    do
      note_name <- parse_note_name
      octave <- parse_int
      return (Note octave note_name)
  parse_note_name :: Parser Note_name
  parse_note_name = construct_note_name <$> (Note_name' <$> parse_natural_note_name <*> parse_accidental)
  parse_notes :: Parser [Note]
  parse_notes = parse_angular (parse_many parse_note)
  parse_sequential :: Parser [Simultaneous]
  parse_sequential = parse_many parse_simultaneous
  parse_sharp :: Parser Accidental
  parse_sharp =
    do
      parse_token Sharp_token
      return Sharp
  parse_simple_length :: Parser Rat
  parse_simple_length =
    do
      base_length <- parse_base_length
      dots <- parse_dots
      return (base_length * (2 - 1 / 2 ^ dots))
  parse_simultaneous :: Parser Simultaneous
  parse_simultaneous = Simultaneous <$> parse_notes <*> parse_length
  parse_some :: Parser t -> Parser [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  parse_token :: Token -> Parser ()
  parse_token token = parse_token' (certain_token token)
  parse_token' :: (Token -> Maybe t) -> Parser t
  parse_token' f =
    do
      token : tokens <- get
      case f token of
        Nothing -> throwError ()
        Just x ->
          do
            tell 1
            put tokens
            return x
  runWS :: WS output state t -> state -> (t, state, output)
  runWS f st = runRWS f () st
  runWST :: WST output state f t -> state -> f (t, state, output)
  runWST f = runRWST f ()
  -- | The length of a note sequence.
  sequential_length :: [Simultaneous] -> Rat
  sequential_length sequential = sum (simultaneous_length <$> sequential)
  -- | The length of a collection of notes.
  simultaneous_length :: Simultaneous -> Rat
  simultaneous_length (Simultaneous _ len) = len
  -- | Discards the topmost division of the bar. For example, 1\/1 is transformed into 1\/2, 2\/2 into 1\/2, 3\/4 into 1\/4,
  -- 4\/4 into 2\/4, 6\/8 into 3\/8, 9\/16 into 3\/16.
  subdivision :: Time -> Time
  subdivision (Time numerator denominator) =
    case numerator of
      [] -> Time [] (2 * denominator)
      _ : numerator' -> Time numerator' denominator
  template_error :: String -> t
  template_error err = error ("Template error. " ++ err)
  tokenise :: String -> [Token]
  tokenise text =
    let
      ((), _, tokens) = runWS tokenise' (classify_char <$> text)
    in
      tokens
  tokenise' :: Tokeniser ()
  tokenise' =
    do
      maybe_char <- get_char 0
      maybe_char' <- get_char 1
      case maybe_char of
        Nothing -> add_token End_token
        Just char ->
          do
            case char of
              Delimiter_char token ->
                do
                  add_token token
                  next_char
              Nat_char c ->
                case (c, maybe_char') of
                  ('0', Just (Nat_char _)) -> template_error "Int starting with zero."
                  _ -> tokenise_nat
              Negation_char ->
                case maybe_char' of
                  Just (Nat_char c) ->
                    case c of
                      '0' -> template_error "Negation of int starting with zero."
                      _ ->
                        do
                          add_token Negation_token
                          next_char
                  _ -> template_error "Standalone negation sign."
              Space_char -> next_char
            tokenise'
  tokenise_nat :: Tokeniser ()
  tokenise_nat =
    do
      i <- gather_nat
      add_token (Nat_token (read i))
  wst :: (state -> f (t, state, output)) -> WST output state f t
  wst f = RWST (\() -> f)
--------------------------------------------------------------------------------------------------------------------------------