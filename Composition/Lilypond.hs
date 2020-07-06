--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: A module for generating Lilypond files.

This module contains the data structures and the function for generating Lilypond files from note sequences.
-}
module Composition.Lilypond (
  Basic_clef (..),
  Bracket (..),
  Clef (..),
  Field (..),
  Instrument_stave (..),
  Part (..),
  Part_header_field_name (..),
  Score (..),
  Score_header_field_name (..),
  Stave (..),
  lilypond) where
  import Composition.Notes (
    Accidental (..),
    Natural_note_name (..),
    Note (..),
    Note_name (..),
    Note_name' (..),
    Rat,
    Simultaneous (..),
    Time (..),
    Time_and_position (..),
    deconstruct_note_name,
    measure_length,
    sequential_length,
    simultaneous_length,
    subdivision)
  import Control.Monad (join)
  import Data.Char (isPrint)
  import Data.Fixed (mod')
  import Data.List (intercalate)
  import Data.Maybe (catMaybes)
  import Data.Ratio ((%), denominator, numerator)
  import System.Process (callCommand)
  -- | Basic clefs without octave transposition.
  data Basic_clef = Sub_bass | Bass | Baritone_F | Baritone_C | Tenor | Alto | Soprano | Mezzosoprano | Treble | French
  -- | Staves can be separate from other staves or grouped with some of the other staves in a bracket. The curly bracket groups
  -- staves of the same instrument, for example, the left- and right-hand part for the keyboard. The square bracket groups
  -- several instruments in the same family, for example, the strings.
  data Bracket = Curly_bracket (Maybe String) [Stave] | Single Instrument_stave | Square_bracket [Instrument_stave]
  -- | Clefs with octave transposition. The second argument indicates the number of octaves by which the clef is transposed.
  data Clef = Clef Basic_clef Int
  type Err = Either String
  -- | The type that generalises score and part header fields.
  data Field field_name = Field field_name String
  -- | A stave with optional instrument specification.
  data Instrument_stave = Instrument_stave (Maybe String) Stave
  -- | Each part can have a different time signature, tempo and instrumentation. The first argument is the list of header fields
  -- that may include, for example, the title of the part. The second argument is the list of accidentals. For example, the key
  -- signature of C minor would be @[E_flat, A_flat, B_flat]@.
  data Part = Part [Field Part_header_field_name] [Note_name] Time_and_position [Bracket]
  -- | Lilypond header fields that apply to only one part of the whole document.
  data Part_header_field_name = Opus | Piece
  -- | The first argument is the list of score header fields that may include, for example, the composer, the dedication, the
  -- instrument, the subtitle and the title.
  data Score = Score [Field Score_header_field_name] [Part]
  -- | Lilypond header fields that apply to the whole document.
  data Score_header_field_name =
    Arranger | Composer | Copyright | Dedication | Instrument | Meter | Poet | Subsubtitle | Subtitle | Tagline | Title
  -- | A stave consists of one homorhythmic note sequence. Heterorhythmic voices have to be notated on separate staves.
  data Stave = Stave Clef [Simultaneous]
  deriving instance Eq Part_header_field_name
  deriving instance Eq Score_header_field_name
  deriving instance Show Basic_clef
  deriving instance Show Bracket
  deriving instance Show Clef
  deriving instance Show field_name => Show (Field field_name)
  deriving instance Show Instrument_stave
  deriving instance Show Part
  deriving instance Show Part_header_field_name
  deriving instance Show Score
  deriving instance Show Score_header_field_name
  deriving instance Show Stave
  all_different :: Eq a => [a] -> Bool
  all_different x =
    case x of
      [] -> True
      y : z -> all ((/=) y) z && all_different z
  bracket_length :: Bracket -> Err (Maybe Rat)
  bracket_length bracket =
    case bracket of
      Curly_bracket _ staves -> check_lengths (stave_length <$> staves)
      Single instrument_stave -> Right (Just (instrument_stave_length instrument_stave))
      Square_bracket instrument_staves -> check_lengths (instrument_stave_length <$> instrument_staves)
  check :: String -> Bool -> Err ()
  check err condition =
    case condition of
      False -> Left err
      True -> Right ()
  check_bracket_lengths :: [Bracket] -> Err ()
  check_bracket_lengths brackets =
    do
      lengths <- traverse bracket_length brackets
      _ <- check_lengths (catMaybes lengths)
      Right ()
  check_lengths :: [Rat] -> Err (Maybe Rat)
  check_lengths lengths =
    case lengths of
      [] -> Right Nothing
      len : lengths' ->
        do
          check "Stave length mismatch." (all ((==) len) lengths')
          Right (Just len)
  check_range :: Ord t => String -> t -> t -> t -> Err ()
  check_range typ min_t max_t x = check (typ ++ " out of range.") (min_t <= x && max_t >= x)
  from_right :: Either t u -> u
  from_right x =
    case x of
      Left _ -> undefined
      Right y -> y
  instrument_stave_length :: Instrument_stave -> Rat
  instrument_stave_length (Instrument_stave _ stave) = stave_length stave
  is_power_of_two :: Int -> Bool
  is_power_of_two i =
    case i of
      1 -> True
      _ -> even i && is_power_of_two (div i 2)
  -- | Encodes the score in Lilypond format, writes it to the specified file and generates the pdf by calling lilypond.
  lilypond :: String -> Score -> IO ()
  lilypond file_name score =
    do
      let file_name_ly = file_name ++ ".ly"
      case write_score score of
        Left err -> putStrLn ("Lilypond error. " ++ err)
        Right score' ->
          do
            writeFile file_name_ly score'
            callCommand ("lilypond" ++ " " ++ file_name_ly)
  lg :: Int -> Int
  lg i =
    case i of
      1 -> 0
      _ -> 1 + lg (div i 2)
  max_denominator :: Int
  max_denominator = 2 ^ (negate min_lg)
  min_length :: Rat
  min_length = 1 % max_denominator
  min_lg :: Integer
  min_lg = -7
  split :: Time -> Rat -> Rat -> [(Rat, Rat)]
  split time position len =
    let
      len' = measure_length time - position
      position' = mod' position (measure_length (subdivision time))
    in
      case len > len' of
        False -> [(position', len)]
        True -> (position', len') : split time 0 (len - len')
  stave_length :: Stave -> Rat
  stave_length (Stave _ sequential) = sequential_length sequential
  write_accidentals :: [Note_name] -> Err String
  write_accidentals accidentals =
    do
      accidentals' <- traverse (write_key_accidental (deconstruct_note_name <$> accidentals)) [C_natural .. B_natural]
      Right ("\\key" ++ " " ++ write_note_name C ++ " " ++ "#" ++ "`" ++ write_round (intercalate " " accidentals'))
  write_angular :: String -> String
  write_angular = write_brackets "<" ">"
  write_angular_2 :: String -> String
  write_angular_2 = write_brackets "<<" ">>"
  write_bar_line :: String
  write_bar_line = "\\bar" ++ " " ++ "\"|.\""
  write_basic_clef :: Basic_clef -> String
  write_basic_clef basic_clef =
    case basic_clef of
      Sub_bass -> "subbass"
      Bass -> "bass"
      Baritone_F -> "baritonevarF"
      Baritone_C -> "baritone"
      Tenor -> "tenor"
      Alto -> "alto"
      Soprano -> "soprano"
      Mezzosoprano -> "mezzosoprano"
      Treble -> "violin"
      French -> "french"
  write_bracket :: Time_and_position -> Bracket -> Err String
  write_bracket time_and_initial_position bracket =
    case bracket of
      Curly_bracket instrument staves ->
        do
          instrument' <- write_instrument instrument
          staves' <- traverse (write_stave time_and_initial_position Nothing) staves
          Right ("\\new" ++ " " ++ "PianoStaff" ++ " " ++ instrument' ++ " " ++ write_angular_2 (intercalate " " staves'))
      Single instrument_stave -> write_instrument_stave time_and_initial_position instrument_stave
      Square_bracket instrument_staves ->
        do
          instrument_staves' <- traverse (write_instrument_stave time_and_initial_position) instrument_staves
          Right ("\\new" ++ " " ++ "StaffGroup" ++ " " ++ write_angular_2 (intercalate " " instrument_staves'))
  write_brackets :: String -> String -> String -> String
  write_brackets left_bracket right_bracket x = left_bracket ++ x ++ right_bracket
  write_char :: Char -> Err String
  write_char c =
    do
      check "Invalid character." (isPrint c && not (elem c ['\t', '\v', '\f', '\r']))
      Right
        (case c of
          '\n' -> "\\n"
          '"' -> "\""
          '\\' -> "\\\\"
          _ -> [c])
  write_clef :: Clef -> String
  write_clef clef = "\\clef" ++ " " ++ from_right (write_quotes (write_clef' clef))
  write_clef' :: Clef -> String
  write_clef' (Clef basic_clef octave) = write_basic_clef basic_clef ++ write_clef_octave octave
  write_clef_octave :: Int -> String
  write_clef_octave octave =
    case compare octave 0 of
      LT -> "_" ++ write_clef_octave' (negate octave)
      EQ -> ""
      GT -> "^" ++ write_clef_octave' octave
  write_clef_octave' :: Int -> String
  write_clef_octave' octave = show (8 * octave - 1)
  write_complex_length :: Time -> Rat -> Rat -> [String]
  write_complex_length time position len = split time position len >>= write_simple_or_complex_length (subdivision time)
  write_curly :: String -> String
  write_curly = write_brackets "{" "}"
  write_denominator :: Int -> Err String
  write_denominator den =
    do
      check_range "Time signature denominator" 1 max_denominator den
      check "Time signature denominator not a power of two." (is_power_of_two den)
      Right (show den)
  write_eq :: String -> String -> String
  write_eq x y = x ++ " " ++ "=" ++ " " ++ y
  write_field :: (field_name -> String) -> Field field_name -> Err String
  write_field write_field_name (Field field_name value) = write_eq (write_field_name field_name) <$> write_quotes value
  write_header :: Eq field_name => (field_name -> String) -> [Field field_name] -> Err String
  write_header write_field_name fields =
    write_maybe
      (write_header' write_field_name)
      (case fields of
        [] -> Nothing
        _ -> Just fields)
  write_header' :: Eq field_name => (field_name -> String) -> [Field field_name] -> Err String
  write_header' write_field_name fields =
    do
      check "Conflicting header fields." (all_different ((\(Field field_name _) -> field_name) <$> fields))
      fields' <- traverse (write_field write_field_name) fields
      Right ("\\header" ++ " " ++ write_curly (intercalate " " fields'))
  write_initial_position :: Time -> Rat -> Err [String]
  write_initial_position time initial_position =
    do
      check_range "Initial position" 0 (measure_length time - min_length) initial_position
      Right
        (case initial_position of
          0 -> []
          _ ->
            [
              "\\partial" ++
              " " ++
              show max_denominator ++
              "*" ++
              show (numerator ((measure_length time - initial_position) / min_length))])
  write_instrument :: Maybe String -> Err String
  write_instrument = write_maybe write_instrument'
  write_instrument' :: String -> Err String
  write_instrument' instrument =
    do
      instrument' <- write_quotes instrument
      Right ("\\with" ++ " " ++ write_curly (write_eq "instrumentName" instrument'))
  write_instrument_stave :: Time_and_position -> Instrument_stave -> Err String
  write_instrument_stave time_and_initial_position (Instrument_stave instrument stave) =
    write_stave time_and_initial_position instrument stave
  write_key_accidental :: [Note_name'] -> Natural_note_name -> Err String
  write_key_accidental accidentals natural_note_name =
    do
      let accidentals' = filter (\(Note_name' natural_note_name' _) -> natural_note_name == natural_note_name') accidentals
      check "Conflicting accidentals in key signature." (2 > length accidentals')
      Right
        (write_round
          (
            show (fromEnum natural_note_name) ++
            " " ++
            "." ++
            " " ++
            "," ++
            write_key_accidental'
              (case accidentals of
                [Note_name' _ accidental] -> accidental
                _ -> Natural)))
  write_key_accidental' :: Accidental -> String
  write_key_accidental' accidental =
    case accidental of
      Flat -> "FLAT"
      Natural -> "NATURAL"
      Sharp -> "SHARP"
  write_key_and_time :: [Note_name] -> Time_and_position -> Err String
  write_key_and_time accidentals (Time_and_position time initial_position) =
    do
      accidentals' <- write_accidentals accidentals
      time' <- write_time time
      initial_position' <- write_initial_position time initial_position
      Right
        (write_eq
          "Key_and_time"
          (write_curly (intercalate " " ([accidentals', "\\numericTimeSignature"] ++ initial_position' ++ [time']))))
  write_language :: String
  write_language = "\\include" ++ " " ++ from_right (write_quotes "english.ly")
  write_maybe :: (t -> Err String) -> Maybe t -> Err String
  write_maybe write_t maybe_x =
    case maybe_x of
      Nothing -> Right ""
      Just x -> write_t x
  write_natural_note_name :: Natural_note_name -> String
  write_natural_note_name natural_note_name =
    case natural_note_name of
      C_natural -> "c"
      D_natural -> "d"
      E_natural -> "e"
      F_natural -> "f"
      G_natural -> "g"
      A_natural -> "a"
      B_natural -> "b"
  write_note :: Note -> String
  write_note (Note octave note_name) = write_note_name note_name ++ write_note_octave octave
  write_note_name :: Note_name -> String
  write_note_name note_name =
    let
      Note_name' natural_note_name accidental = deconstruct_note_name note_name
    in
      write_natural_note_name natural_note_name ++ write_note_name_accidental accidental
  write_note_name_accidental :: Accidental -> String
  write_note_name_accidental accidental =
    case accidental of
      Flat -> "f"
      Natural -> ""
      Sharp -> "s"
  write_note_octave :: Int -> String
  write_note_octave octave =
    case compare octave 3 of
      LT -> replicate (3 - octave) ','
      EQ -> ""
      GT -> replicate (octave - 3) '\''
  write_notes :: [Note] -> String
  write_notes notes =
    case notes of
      [note] -> write_note note
      _ -> write_angular (intercalate " " (write_note <$> notes))
  write_numerator :: [Int] -> Err String
  write_numerator num =
    do
      check "Invalid time signature numerator." (all ((<) 1) num)
      Right (show (product num))
  write_option :: String -> String -> String
  write_option option value = "#" ++ write_round ("ly:set-option" ++ " " ++ "'" ++ option ++ " " ++ value)
  write_options :: String
  write_options = intercalate " " [write_option "delete-intermediate-files" "#t", write_option "no-point-and-click" "#t"]
  write_part :: Part -> Err String
  write_part (Part header accidentals time_and_initial_position brackets) =
    do
      header' <- write_header write_part_header_field_name header
      key_and_time <- write_key_and_time accidentals time_and_initial_position
      check_bracket_lengths brackets
      brackets' <- traverse (write_bracket time_and_initial_position) brackets
      Right
        (key_and_time ++ " " ++ "\\score" ++ " " ++ write_curly (header' ++ " " ++ write_angular_2 (intercalate " " brackets')))
  write_part_header_field_name :: Part_header_field_name -> String
  write_part_header_field_name field_name =
    case field_name of
      Opus -> "opus"
      Piece -> "piece"
  write_quotes :: String -> Err String
  write_quotes text =
    do
      text' <- traverse write_char text
      Right (write_brackets "\"" "\"" (join text'))
  write_round :: String -> String
  write_round = write_brackets "(" ")"
  write_score :: Score -> Err String
  write_score (Score header parts) =
    do
      header' <-
        write_header
        write_score_header_field_name
        (case any (\(Field field_name _) -> field_name == Tagline) header of
          False -> Field Tagline "" : header
          True -> header)
      parts' <- traverse write_part parts
      Right (write_options ++ " " ++ write_language ++ " " ++ header' ++ " " ++ intercalate " " parts')
  write_score_header_field_name :: Score_header_field_name -> String
  write_score_header_field_name field_name =
    case field_name of
      Arranger -> "arranger"
      Composer -> "composer"
      Copyright -> "copyright"
      Dedication -> "dedication"
      Instrument -> "instrument"
      Meter -> "meter"
      Poet -> "poet"
      Subsubtitle -> "subsubtitle"
      Subtitle -> "subtitle"
      Tagline -> "tagline"
      Title -> "title"
  write_sequential :: Time -> Rat -> [Simultaneous] -> Err [String]
  write_sequential time position sequential =
    case sequential of
      [] -> Right []
      simultaneous : sequential' ->
        (
          (:) <$>
          write_simultaneous time position simultaneous <*>
          write_sequential time (mod' (position + simultaneous_length simultaneous) (measure_length time)) sequential')
  write_simple_length :: Rat -> Maybe String
  write_simple_length len =
    case is_power_of_two (1 + numerator len) && numerator len < 2 * denominator len of
      False -> Nothing
      True ->
        let
          dots = lg (1 + numerator len) - 1
        in
          Just (show ((2 :: Integer) ^ (lg (denominator len) - dots)) ++ replicate dots '.')
  write_simple_or_complex_length :: Time -> (Rat, Rat) -> [String]
  write_simple_or_complex_length time (position, len) =
    case write_simple_length len of
      Nothing -> write_complex_length time position len
      Just len' -> [len']
  write_simultaneous :: Time -> Rat -> Simultaneous -> Err String
  write_simultaneous time position (Simultaneous notes len) =
    do
      check "Non-positive note length." (0 < len)
      check "Tuplets are not supported." (is_power_of_two (denominator len))
      check ("Notes shorter than 1/" ++ show max_denominator ++ " are not supported.") (max_denominator >= denominator len)
      let lengths = write_complex_length time position len
      Right
        (case notes of
          [] -> intercalate " " ((++) "r" <$> lengths)
          _ -> write_notes notes ++ intercalate "~" lengths)
  write_stave :: Time_and_position -> Maybe String -> Stave -> Err String
  write_stave (Time_and_position time initial_position) instrument (Stave clef sequential) =
    do
      instrument' <- write_instrument instrument
      sequential' <- write_sequential time initial_position sequential
      Right
        (
          "\\new" ++
          " " ++
          "Staff" ++
          " " ++
          instrument' ++
          " " ++
          write_curly
            ("\\Key_and_time" ++ " " ++ write_clef clef ++ " " ++ intercalate " " sequential' ++ " " ++ write_bar_line))
  write_time :: Time -> Err String
  write_time time =
    do
      time' <- write_time' time
      Right ("\\time" ++ " " ++ time')
  write_time' :: Time -> Err String
  write_time' (Time num den) =
    do
      num' <- write_numerator num
      den' <- write_denominator den
      Right (num' ++ "/" ++ den')
--------------------------------------------------------------------------------------------------------------------------------