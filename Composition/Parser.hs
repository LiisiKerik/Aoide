{-|
Parse the custom score file format.
-}
module Composition.Parser (parse) where
  import Composition.Errors
  import Composition.Notes
  import Composition.Score
  import Control.Applicative
  import Control.Monad.Except
  import Control.Monad.Trans.Except
  import Data.Char
  import Data.Functor
  import Data.List as List
  import Data.Map.Strict
  import Data.Maybe
  import Data.Set
  import Data.Tuple
  import Parser.Files
  import Parser.Locations
  import Parser.Parser
  import Parser.Utilities
  import Text.Read
  data Char_class =
    Delimiter_char Token |
    Invalid_char |
    Letter_char Char |
    Minus_char |
    Nonzero_nat_char Char |
    Newline_char |
    Quote_char |
    Whitespace_char |
    Zero_char
  type Parser = Parser' Token Error
  data Token =
    Clef_name_name_token |
    Clef_name_value_token Clef_name |
    Clef_token |
    Dot_token |
    Eq_token |
    Header_field_token Header_field |
    Header_token |
    Initial_position_token |
    Instrument_and_staves_token |
    Instrument_name_token |
    Key_token |
    Left_curly_bracket_token |
    Left_square_bracket_token |
    MIDI_instrument_token |
    Negative_int_token Int |
    Note_name_token Note_name |
    Part_token |
    Parts_token |
    Percussion_clef_token |
    Pitched_clef_token |
    Pitched_token |
    Positive_int_token Int |
    Right_curly_bracket_token |
    Right_square_bracket_token |
    Score_token |
    Short_instrument_name_token |
    Slash_token |
    Stave_token |
    Stave_groups_token |
    Staves_token |
    Tempo_token |
    Text_token String |
    Tie_token |
    Time_name_token |
    Time_value_token |
    Title_token |
    Tracks_token |
    Transposition_token |
    Unpitched_note_token |
    Unpitched_token |
    Velocity_token |
    Zero_token
  type Tokeniser = Tokeniser' Char_class Token Error
  deriving instance Eq Char_class
  deriving instance Eq Token
  deriving instance Ord Token
  deriving instance Show Char_class
  deriving instance Show Token
  classify_char :: Char -> Char_class
  classify_char c =
    case c of
      '\n' -> Newline_char
      ' ' -> Whitespace_char
      _ | elem c "!#$%&'()*,:;<>?@^_`|~" || isLetter c -> Letter_char c
      '"' -> Quote_char
      _ | elem c (fst <$> delimiters) -> Delimiter_char (fromJust (List.lookup c delimiters))
      '-' -> Minus_char
      '0' -> Zero_char
      _ | isDigit c && c /= '0' -> Nonzero_nat_char c
      _ -> Invalid_char
  clef_name_token :: Token -> Maybe Clef_name
  clef_name_token token =
    case token of
      Clef_name_value_token clef_name -> Just clef_name
      _ -> Nothing
  construct_header :: [(Header_field, String)] -> Either (Location -> Error) (Map Header_field String)
  construct_header header =
    case construct_map header of
      Nothing -> Left (Duplicate_header_fields <$> Just)
      Just header' -> Right header'
  construct_key :: [Note_name] -> Either (Location -> Error) (Set Note_name)
  construct_key key =
    case construct_set key of
      Nothing -> Left Duplicate_note_names_in_key_signature
      Just key' -> Right key'
  construct_notes :: [Note note_type] -> Either (Location -> Error) (Notes note_type)
  construct_notes notes =
    case construct_set notes of
      Nothing -> Left Duplicate_notes
      Just notes' -> Right (Notes (notes'))
  construct_word :: String -> Either (Location -> Error) Token
  construct_word word =
    (case construct_clef_name <|> construct_header_field <|> construct_keyword <|> construct_note_name' of
      Nothing -> Left (Invalid_word_score word)
      Just token -> Right token) where
    construct_clef_name :: Maybe Token
    construct_clef_name = Clef_name_value_token <$> readMaybe word
    construct_header_field :: Maybe Token
    construct_header_field = Header_field_token <$> readMaybe word
    construct_keyword :: Maybe Token
    construct_keyword =
      case word of
        "Instrument_and_staves" -> Just Instrument_and_staves_token
        "Part" -> Just Part_token
        "Percussion_clef" -> Just Percussion_clef_token
        "Pitched" -> Just Pitched_token
        "Pitched_clef" -> Just Pitched_clef_token
        "Score" -> Just Score_token
        "Stave" -> Just Stave_token
        "Time" -> Just Time_value_token
        "Unpitched" -> Just Unpitched_token
        "clef" -> Just Clef_token
        "clef_name" -> Just Clef_name_name_token
        "header" -> Just Header_token
        "initial_position" -> Just Initial_position_token
        "instrument_name" -> Just Instrument_name_token
        "key" -> Just Key_token
        "midi_instrument" -> Just MIDI_instrument_token
        "parts" -> Just Parts_token
        "short_instrument_name" -> Just Short_instrument_name_token
        "stave_groups" -> Just Stave_groups_token
        "staves" -> Just Staves_token
        "tempo" -> Just Tempo_token
        "time" -> Just Time_name_token
        "title" -> Just Title_token
        "tracks" -> Just Tracks_token
        "transposition" -> Just Transposition_token
        "velocity" -> Just Velocity_token
        "x" -> Just Unpitched_note_token
        _ -> Nothing
    construct_note_name' :: Maybe Token
    construct_note_name' =
      case word of
        "" -> Nothing
        natural_note_name : accidental ->
          do
            natural_note_name' <- read_natural_note_name natural_note_name
            accidental' <- read_accidental accidental
            Note_name_token <$> construct_note_name natural_note_name' accidental'
  convert_basic_length :: Int -> Either (Location -> Error) Basic_length
  convert_basic_length len =
    case denominator_to_basic_length len of
      Nothing -> Left (Invalid_note_length len)
      Just len' -> Right len'
  convert_midi_instrument :: Int -> Either (Location -> Error) MIDI_instrument
  convert_midi_instrument midi_instrument =
    do
      check (MIDI_instrument_code_is_out_of_range_score midi_instrument) (between 0 127 midi_instrument)
      Right (fromIntegral midi_instrument)
  convert_time_numerator_factor :: Int -> Either (Location -> Error) Time_numerator_factor
  convert_time_numerator_factor time_numerator_factor =
    case int_to_time_numerator_factor time_numerator_factor of
      Nothing -> Left (Invalid_time_numerator_factor time_numerator_factor)
      Just time_numerator_factor' -> Right time_numerator_factor'
  convert_velocity :: Int -> Either (Location -> Error) Velocity
  convert_velocity velocity =
    do
      check Velocity_out_of_range (between 0 (fromIntegral max_velocity) velocity)
      Right (fromIntegral velocity)
  delimiter_char :: Char_class -> Maybe Token
  delimiter_char char_class =
    case char_class of
      Delimiter_char token -> Just token
      _ -> Nothing
  delimiters :: [(Char, Token)]
  delimiters =
    [
      ('+', Tie_token),
      ('.', Dot_token),
      ('/', Slash_token),
      ('=', Eq_token),
      ('[', Left_square_bracket_token),
      (']', Right_square_bracket_token),
      ('{', Left_curly_bracket_token),
      ('}', Right_curly_bracket_token)]
  header_field_token :: Token -> Maybe Header_field
  header_field_token token =
    case token of
      Header_field_token header_field -> Just header_field
      _ -> Nothing
  letter_char :: Char_class -> Maybe Char
  letter_char char_class =
    case char_class of
      Letter_char c -> Just c
      _ -> Nothing
  nat_char :: Char_class -> Maybe Char
  nat_char char_class =
    case char_class of
      Zero_char -> Just '0'
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  negative_int_token :: Token -> Maybe Int
  negative_int_token token =
    case token of
      Negative_int_token i -> Just i
      _ -> Nothing
  next_location :: Char_class -> Location -> Location
  next_location char_class =
    case char_class of
      Newline_char -> next_line
      _ -> next_char
  nonzero_nat_char :: Char_class -> Maybe Char
  nonzero_nat_char char_class =
    case char_class of
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  note_name_token :: Token -> Maybe Note_name
  note_name_token token =
    case token of
      Note_name_token note_name -> Just note_name
      _ -> Nothing
  -- | Read the score from a .aoi file.
  parse :: File_path -> ExceptT Error IO Score
  parse file_path =
    do
      score <- read_file "aoi" readFile File_error file_path
      except (fromJust (parse' classify_char next_location tokenise parse_score Parse_error_score score))
  parse_basic_length :: Parser Basic_length
  parse_basic_length = fmap_filter_parser convert_basic_length parse_positive_int
  parse_clef_name :: Parser Clef_name
  parse_clef_name = parse_token' clef_name_token
  parse_curly_brackets :: Parser t -> Parser t
  parse_curly_brackets = parse_brackets Left_curly_bracket_token Right_curly_bracket_token
  parse_dot :: Parser Dot
  parse_dot =
    do
      parse_token Dot_token
      return Dot
  parse_eq :: Parser ()
  parse_eq = parse_token Eq_token
  parse_field :: Token -> Parser t -> Parser t
  parse_field name parse_t =
    do
      parse_token name
      parse_eq
      parse_t
  parse_header :: Parser (Map Header_field String)
  parse_header = fmap_filter_parser construct_header (parse_list' parse_header_field)
  parse_header_field :: Parser (Header_field, String)
  parse_header_field =
    do
      name <- parse_header_field_name
      parse_eq
      value <- parse_text
      return (name, value)
  parse_header_field_name :: Parser Header_field
  parse_header_field_name = parse_token' header_field_token
  parse_initial_position :: Parser Initial_position
  parse_initial_position = parse_zero_initial_position <+> parse_nonzero_initial_position
  parse_instrument_and_staves :: Parser (Pitched_or_unpitched Instrument_and_staves)
  parse_instrument_and_staves =
    Pitched <$> parse_instrument_and_staves' Branch_pitched <+> Unpitched <$> parse_instrument_and_staves' Branch_unpitched
  parse_instrument_and_staves' :: forall note_type . Branch note_type -> Parser (Instrument_and_staves note_type)
  parse_instrument_and_staves' branch =
    do
      parse_pitched_or_unpitched
      parse_struct
        Instrument_and_staves_token
        (do
          instrument_name <- parse_field Instrument_name_token parse_text
          short_instrument_name <- parse_field Short_instrument_name_token parse_text
          midi_instrument <- parse_field MIDI_instrument_token parse_midi_instrument
          velocity <- parse_field Velocity_token parse_velocity
          staves <- parse_field Staves_token (parse_list' parse_stave)
          return (Instrument_and_staves {instrument_name, short_instrument_name, midi_instrument, velocity, staves})) where
    parse_clef :: Parser (Clef note_type)
    parse_clef =
      case branch of
        Branch_pitched -> parse_pitched_clef
        Branch_unpitched -> parse_unpitched_clef
    parse_event :: Parser (Event note_type)
    parse_event = parse_event' <+> parse_triplet
    parse_event' :: Parser (Event note_type)
    parse_event' = Event <$> parse_notes <*> parse_length
    parse_events :: Parser [Event note_type]
    parse_events = parse_list' parse_event
    parse_note :: Parser (Note note_type)
    parse_note =
      case branch of
        Branch_pitched -> parse_pitched_note
        Branch_unpitched -> parse_unpitched_note
    parse_notes :: Parser (Notes note_type)
    parse_notes = parse_notes' <+> parse_tie
    parse_notes' :: Parser (Notes note_type)
    parse_notes' = fmap_filter_parser construct_notes (parse_list' parse_note)
    parse_one_track :: Parser (Tracks note_type)
    parse_one_track = One_track <$> parse_events
    parse_pitched_or_unpitched :: Parser ()
    parse_pitched_or_unpitched = parse_token' pitched_or_unpitched_token
    parse_stave :: Parser (Stave note_type)
    parse_stave =
      parse_struct
        Stave_token
        (do
          clef <- parse_field Clef_token parse_clef
          tracks <- parse_field Tracks_token parse_tracks
          return (Stave {clef, tracks}))
    parse_tracks :: Parser (Tracks note_type)
    parse_tracks = parse_square_brackets (parse_one_track <+> parse_two_tracks)
    parse_triplet :: Parser (Event note_type)
    parse_triplet = Triplet <$> parse_events
    parse_two_tracks :: Parser (Tracks note_type)
    parse_two_tracks = Two_tracks <$> parse_events <*> parse_events
    pitched_or_unpitched_token :: Token -> Maybe ()
    pitched_or_unpitched_token token =
      case (branch, token) of
        (Branch_pitched, Pitched_token) -> Just ()
        (Branch_unpitched, Unpitched_token) -> Just ()
        _ -> Nothing
  parse_int :: Parser Int
  parse_int = parse_negative_int <+> parse_nat
  parse_key :: Parser Key
  parse_key = fmap_filter_parser construct_key (parse_list' parse_note_name)
  parse_length :: Parser Length
  parse_length = Length <$> parse_basic_length <*> parse_many parse_dot
  parse_list' :: Parser t -> Parser [t]
  parse_list' parse_t = parse_square_brackets (parse_many parse_t)
  parse_midi_instrument :: Parser MIDI_instrument
  parse_midi_instrument = fmap_filter_parser convert_midi_instrument parse_nat
  parse_nat :: Parser Int
  parse_nat = parse_zero <+> parse_positive_int
  parse_negative_int :: Parser Int
  parse_negative_int = parse_token' negative_int_token
  parse_nonzero_initial_position :: Parser Initial_position
  parse_nonzero_initial_position =
    do
      num <- parse_positive_int
      parse_slash
      den <- parse_basic_length
      return (Initial_position num den)
  parse_note_name :: Parser Note_name
  parse_note_name = parse_token' note_name_token
  parse_part :: Parser Part
  parse_part =
    parse_struct
      Part_token
      (do
        title <- parse_field Title_token parse_text
        key <- parse_field Key_token parse_key
        time <- parse_field Time_name_token parse_time
        initial_position <- parse_field Initial_position_token parse_initial_position
        tempo <- parse_field Tempo_token parse_positive_int
        stave_groups <- parse_field Stave_groups_token (parse_list' (parse_list' parse_instrument_and_staves))
        return (Part {title, key, time, initial_position, tempo, stave_groups}))
  parse_pitched_clef :: Parser (Clef Pitched)
  parse_pitched_clef =
    parse_struct
      Pitched_clef_token
      (do
        clef_name <- parse_field Clef_name_name_token parse_clef_name
        transposition <- parse_field Transposition_token parse_int
        return (Pitched_clef {clef_name, transposition}))
  parse_pitched_note :: Parser (Note Pitched)
  parse_pitched_note = flip Pitched_note <$> parse_note_name <*> parse_int
  parse_positive_int :: Parser Int
  parse_positive_int = parse_token' positive_int_token
  parse_score :: Parser Score
  parse_score =
    parse_struct
      Score_token
      (do
        header <- parse_field Header_token parse_header
        parts <- parse_field Parts_token (parse_list' parse_part)
        return (Score {header, parts}))
  parse_slash :: Parser ()
  parse_slash = parse_token Slash_token
  parse_square_brackets :: Parser t -> Parser t
  parse_square_brackets = parse_brackets Left_square_bracket_token Right_square_bracket_token
  parse_struct :: Token -> Parser t -> Parser t
  parse_struct name parse_fields =
    do
      parse_token name
      parse_curly_brackets parse_fields
  parse_text :: Parser String
  parse_text = parse_token' text_token
  parse_tie :: Parser (Notes note_type)
  parse_tie =
    do
      parse_token Tie_token
      return Tie
  parse_time :: Parser Time
  parse_time =
    do
      parse_token Time_value_token
      num <- parse_list' parse_time_numerator_factor
      den <- parse_basic_length
      return (Time num den)
  parse_time_numerator_factor :: Parser Time_numerator_factor
  parse_time_numerator_factor = fmap_filter_parser convert_time_numerator_factor parse_positive_int
  parse_unpitched_clef :: Parser (Clef Unpitched)
  parse_unpitched_clef =
    do
      parse_token Percussion_clef_token
      return Percussion_clef
  parse_unpitched_note :: Parser (Note Unpitched)
  parse_unpitched_note =
    do
      parse_token Unpitched_note_token
      return Unpitched_note
  parse_velocity :: Parser Velocity
  parse_velocity = fmap_filter_parser convert_velocity parse_nat
  parse_zero :: Parser Int
  parse_zero =
    do
      parse_token Zero_token
      return 0
  parse_zero_initial_position :: Parser Initial_position
  parse_zero_initial_position =
    do
      _ <- parse_zero
      return (Initial_position 0 Whole)
  positive_int_token :: Token -> Maybe Int
  positive_int_token token =
    case token of
      Positive_int_token i -> Just i
      _ -> Nothing
  read_accidental :: String -> Maybe Accidental
  read_accidental accidental = List.lookup accidental (swap <$> accidentals)
  read_natural_note_name :: Char -> Maybe Natural_note_name
  read_natural_note_name natural_note_name = readMaybe (natural_note_name : "_natural")
  text_char :: Char_class -> Maybe Char
  text_char char_class =
    case char_class of
      Delimiter_char token -> List.lookup token (swap <$> delimiters)
      Invalid_char -> Nothing
      Letter_char c -> Just c
      Minus_char -> Just '-'
      Nonzero_nat_char c -> Just c
      Newline_char -> Nothing
      Quote_char -> Nothing
      Whitespace_char -> Just ' '
      Zero_char -> Just '0'
  text_token :: Token -> Maybe String
  text_token token =
    case token of
      Text_token text -> Just text
      _ -> Nothing
  tokenise :: Tokeniser ()
  tokenise = void (parse_many tokenise_1)
  tokenise_1 :: Tokeniser ()
  tokenise_1 =
    tokenise_delimiter <+> tokenise_int <+> tokenise_newline <+> tokenise_text <+> tokenise_whitespace <+> tokenise_word
  tokenise_delimiter :: Tokeniser ()
  tokenise_delimiter = add_token (parse_token' delimiter_char)
  tokenise_int :: Tokeniser ()
  tokenise_int = add_token (tokenise_negative_int <+> tokenise_zero <+> tokenise_positive_int)
  tokenise_negative_int :: Tokeniser Token
  tokenise_negative_int =
    do
      parse_token Minus_char
      i <- tokenise_positive_int'
      return (Negative_int_token (negate i))
  tokenise_newline :: Tokeniser ()
  tokenise_newline = parse_token Newline_char
  tokenise_positive_int :: Tokeniser Token
  tokenise_positive_int = Positive_int_token <$> tokenise_positive_int'
  tokenise_positive_int' :: Tokeniser Int
  tokenise_positive_int' = read <$> ((:) <$> parse_token' nonzero_nat_char <*> parse_many (parse_token' nat_char))
  tokenise_text :: Tokeniser ()
  tokenise_text = add_token (Text_token <$> tokenise_quotes (parse_many (parse_token' text_char)))
  tokenise_quotes :: Tokeniser t -> Tokeniser t
  tokenise_quotes = parse_brackets Quote_char Quote_char
  tokenise_whitespace :: Tokeniser ()
  tokenise_whitespace = parse_token Whitespace_char
  tokenise_word :: Tokeniser ()
  tokenise_word = add_token (fmap_filter_parser construct_word (parse_some (parse_token' letter_char)))
  tokenise_zero :: Tokeniser Token
  tokenise_zero =
    do
      parse_token Zero_char
      return Zero_token