module Main (main) where
  import Composition.Errors
  import Composition.Keyboard
  import Composition.Lilypond
  import Composition.MIDI
  import Composition.Parser
  import Composition.Score
  import Composition.Write
  import Control.Applicative
  import Control.Monad.Except
  import Control.Monad.Trans.Except
  import Data.Char
  import Data.Functor
  import Data.List as List
  import Data.Map.Strict
  import Data.Maybe
  import Data.Tuple
  import Parser.Files
  import Parser.Locations
  import Parser.Parser
  import Parser.Utilities
  import System.Environment
  import Text.Read
  data Char_class =
    Delimiter_char Token |
    File_path_char Char |
    Invalid_char |
    Nonzero_nat_char Char |
    Quote_char |
    Text_char Char |
    Whitespace_char |
    Word_char Char |
    Zero_char
  data Command =
    Keyboard_command {
      playability :: File_path,
      source :: File_path,
      header :: Map Header_field String,
      midi_instrument :: MIDI_instrument,
      destination :: File_path} |
    Lilypond_command {source :: File_path, destination :: File_path} |
    MIDI_command {source :: File_path, destination :: File_path}
  type Parser = Parser' Token Error
  data Token =
    Destination_token |
    Eq_token |
    File_path_token String |
    Header_field_token Header_field |
    Header_token |
    Keyboard_token |
    Left_curly_bracket_token |
    Left_square_bracket_token |
    Lilypond_token |
    MIDI_instrument_token |
    MIDI_token |
    Nat_token Int |
    Playability_token |
    Right_curly_bracket_token |
    Right_square_bracket_token |
    Source_token |
    Text_token String
  type Tokeniser = Tokeniser' Char_class Token Error
  deriving instance Eq Char_class
  deriving instance Eq Token
  deriving instance Show Char_class
  deriving instance Show Command
  deriving instance Show Token
  classify_char :: Char -> Char_class
  classify_char c =
    case c of
      ' ' -> Whitespace_char
      _ | elem c "!#$%&'()*+-,:;<>?@^`|~" -> Text_char c
      '"' -> Quote_char
      _ | elem c "'_" || isLetter c -> Word_char c
      _ | elem c "./" -> File_path_char c
      '0' -> Zero_char
      _ | isDigit c && c /= '0' -> Nonzero_nat_char c
      _ | elem c (fst <$> delimiters) -> Delimiter_char (fromJust (List.lookup c delimiters))
      _ -> Invalid_char
  construct_header :: [(Header_field, String)] -> Either (Location -> Error) (Map Header_field String)
  construct_header header =
    case construct_map header of
      Nothing -> Left (\ _ -> Duplicate_header_fields Nothing)
      Just header' -> Right header'
  construct_word :: String -> Either (Location -> Error) Token
  construct_word word =
    (case construct_header_field <|> construct_keyword of
      Nothing -> Left (\ _ -> Invalid_word_command word)
      Just token -> Right token) where
    construct_header_field :: Maybe Token
    construct_header_field = Header_field_token <$> readMaybe word
    construct_keyword :: Maybe Token
    construct_keyword =
      case word of
        "Keyboard" -> Just Keyboard_token
        "Lilypond" -> Just Lilypond_token
        "MIDI" -> Just MIDI_token
        "destination" -> Just Destination_token
        "header" -> Just Header_token
        "midi_instrument" -> Just MIDI_instrument_token
        "playability" -> Just Playability_token
        "source" -> Just Source_token
        _ -> Nothing
  convert_midi_instrument :: Int -> Either (Location -> Error) MIDI_instrument
  convert_midi_instrument midi_instrument =
    do
      check (\ _ -> MIDI_instrument_code_is_out_of_range_command midi_instrument) (between 0 127 midi_instrument)
      Right (fromIntegral midi_instrument)
  delimiter_char :: Char_class -> Maybe Token
  delimiter_char char_class =
    case char_class of
      Delimiter_char token -> Just token
      _ -> Nothing
  delimiters :: [(Char, Token)]
  delimiters =
    [
      ('=', Eq_token),
      ('[', Left_square_bracket_token),
      (']', Right_square_bracket_token),
      ('{', Left_curly_bracket_token),
      ('}', Right_curly_bracket_token)]
  file_path_char :: Char_class -> Maybe Char
  file_path_char char_class =
    case char_class of
      File_path_char c -> Just c
      Nonzero_nat_char c -> Just c
      Word_char c -> Just c
      Zero_char -> Just '0'
      _ -> Nothing
  file_path_token :: Token -> Maybe String
  file_path_token token =
    case token of
      File_path_token file_path -> Just file_path
      _ -> Nothing
  header_field_token :: Token -> Maybe Header_field
  header_field_token token =
    case token of
      Header_field_token header_field -> Just header_field
      _ -> Nothing
  main :: IO ()
  main =
    do
      args <- getArgs
      result <- runExceptT (main' (intercalate " " args))
      case result of
        Left err -> putStrLn (write_error err)
        Right () -> return ()
  main' :: String -> ExceptT Error IO ()
  main' command =
    (do
      command' <- except parse_command
      case command' of
        Keyboard_command {playability = playability_file_path, source, header, midi_instrument, destination} ->
          do
            playability <- parse_playability playability_file_path
            original_score <- parse source
            keyboard_score <- except (reduction playability header midi_instrument original_score)
            write destination keyboard_score
        Lilypond_command {source, destination} ->
          do
            score <- parse source
            lilypond score destination
        MIDI_command {source, destination} ->
          do
            score <- parse source
            midi score destination) where
    parse_command :: Either Error Command
    parse_command =
      fromJust (parse' classify_char (return next_char) tokenise parse_command' (\ _ -> Parse_error_command) command)
  nat_char :: Char_class -> Maybe Char
  nat_char char_class =
    case char_class of
      Zero_char -> Just '0'
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  nat_token :: Token -> Maybe Int
  nat_token token =
    case token of
      Nat_token i -> Just i
      _ -> Nothing
  nonzero_nat_char :: Char_class -> Maybe Char
  nonzero_nat_char char_class =
    case char_class of
      Nonzero_nat_char c -> Just c
      _ -> Nothing
  parse_command' :: Parser Command
  parse_command' = parse_keyboard <+> parse_lilypond <+> parse_midi
  parse_curly_brackets :: Parser t -> Parser t
  parse_curly_brackets = parse_brackets Left_curly_bracket_token Right_curly_bracket_token
  parse_eq :: Parser ()
  parse_eq = parse_token Eq_token
  parse_field :: Token -> Parser t -> Parser t
  parse_field name parse_t =
    do
      parse_token name
      parse_eq
      parse_t
  parse_file_path :: String -> Parser File_path
  parse_file_path ext = fmap_filter_parser (parse_file_path' (\ err _ -> File_error err) ext) (parse_token' file_path_token)
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
  parse_keyboard :: Parser Command
  parse_keyboard =
    parse_struct
      Keyboard_token
      (do
        playability <- parse_field Playability_token (parse_file_path "key")
        source <- parse_field Source_token (parse_file_path "aoi")
        header <- parse_field Header_token parse_header
        midi_instrument <- parse_field MIDI_instrument_token parse_midi_instrument
        destination <- parse_field Destination_token (parse_file_path "aoi")
        return (Keyboard_command {playability, source, header, midi_instrument, destination}))
  parse_lilypond :: Parser Command
  parse_lilypond =
    parse_struct
      Lilypond_token
      (do
        source <- parse_field Source_token (parse_file_path "aoi")
        destination <- parse_field Destination_token (parse_file_path "ly")
        return (Lilypond_command {source, destination}))
  parse_list' :: Parser t -> Parser [t]
  parse_list' parse_t = parse_square_brackets (parse_many parse_t)
  parse_midi :: Parser Command
  parse_midi =
    parse_struct
      MIDI_token
      (do
        source <- parse_field Source_token (parse_file_path "aoi")
        destination <- parse_field Destination_token (parse_file_path "mid")
        return (MIDI_command {source, destination}))
  parse_midi_instrument :: Parser MIDI_instrument
  parse_midi_instrument = fmap_filter_parser convert_midi_instrument parse_nat
  parse_nat :: Parser Int
  parse_nat = parse_token' nat_token
  parse_square_brackets :: Parser t -> Parser t
  parse_square_brackets = parse_brackets Left_square_bracket_token Right_square_bracket_token
  parse_struct :: Token -> Parser Command -> Parser Command
  parse_struct name parse_fields =
    do
      parse_token name
      parse_curly_brackets parse_fields
  parse_text :: Parser String
  parse_text = parse_token' text_token
  text_char :: Char_class -> Maybe Char
  text_char char_class =
    case char_class of
      Delimiter_char token -> List.lookup token (swap <$> delimiters)
      File_path_char c -> Just c
      Invalid_char -> Nothing
      Nonzero_nat_char c -> Just c
      Quote_char -> Nothing
      Text_char c -> Just c
      Whitespace_char -> Just ' '
      Word_char c -> Just c
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
    tokenise_delimiter <+> ((tokenise_nat <+> tokenise_word) <+ tokenise_file_path) <+> tokenise_text <+> tokenise_whitespace
  tokenise_delimiter :: Tokeniser ()
  tokenise_delimiter = add_token (parse_token' delimiter_char)
  tokenise_file_path :: Tokeniser ()
  tokenise_file_path = add_token (File_path_token <$> parse_some (parse_token' file_path_char))
  tokenise_nat :: Tokeniser ()
  tokenise_nat = add_token (Nat_token <$> (tokenise_zero <+> tokenise_positive_int))
  tokenise_positive_int :: Tokeniser Int
  tokenise_positive_int = read <$> ((:) <$> parse_token' nonzero_nat_char <*> parse_many (parse_token' nat_char))
  tokenise_quotes :: Tokeniser t -> Tokeniser t
  tokenise_quotes = parse_brackets Quote_char Quote_char
  tokenise_text :: Tokeniser ()
  tokenise_text = add_token (Text_token <$> tokenise_quotes (parse_many (parse_token' text_char)))
  tokenise_whitespace :: Tokeniser ()
  tokenise_whitespace = parse_token Whitespace_char
  tokenise_word :: Tokeniser ()
  tokenise_word = add_token (fmap_filter_parser construct_word (parse_some (parse_token' word_char)))
  tokenise_zero :: Tokeniser Int
  tokenise_zero =
    do
      parse_token Zero_char
      return 0
  word_char :: Char_class -> Maybe Char
  word_char char_class =
    case char_class of
      Word_char c -> Just c
      _ -> Nothing
  -- | Writing errors.
  write_error :: Error -> String
  write_error err =
    case err of
      An_event_crosses_the_bar_line -> "An event crosses the bar line."
      Conflicting_accidentals_in_key_signature -> "Conflicting accidentals in key signature."
      Duplicate_header_fields location -> "Duplicate header fields" <-> write_maybe_location location <> "."
      Duplicate_keys location -> "Duplicate keys at" <-> write_location location <> "."
      Duplicate_note_names_in_key_signature location ->
        "Duplicate note name in key signature at" <-> write_location location <> "."
      Duplicate_notes location -> "Duplicate notes at" <-> write_location location <> "."
      Empty_triplet -> "Empty triplet."
      Event_or_part_length_is_out_of_range_for_MIDI -> "Event or part length is out of range for MIDI."
      File_error err' ->
        case err' of
          Failed_to_find_file file_path -> "Failed to find file" <-> file_path <> "."
          Invalid_file_path -> "Invalid file path."
          Unexpected_extension ext -> "Unexpected extension ." <> ext <> "."
      Initial_position_is_out_of_range -> "Initial position is out of range."
      Key_is_out_of_range key location -> "Key" <-> show key <-> "at" <-> write_location location <-> "is out of range."
      Invalid_character_in_text -> "Invalid character in text."
      Invalid_keyword_playability keyword location ->
        "Invalid keyword" <-> keyword <-> "in the playability file" <-> write_location location <> "."
      Invalid_note_length len location -> "Invalid note length" <-> show len <-> "at" <-> write_location location <> "."
      Invalid_note_name natural_note_name accidental location ->
        (
          "Invalid note name" <->
          write_natural_note_name natural_note_name <>
          write_accidental accidental <->
          "at" <->
          write_location location <>
          ".")
      Invalid_time_numerator_factor time_numerator_factor location ->
        "Invalid time numerator factor" <-> show time_numerator_factor <-> "at" <-> write_location location <> "."
      Invalid_word_command word -> "Invalid word" <-> word <-> "in the command."
      Invalid_word_score word location -> "Invalid word" <-> word <-> "in the score" <-> write_location location <> "."
      Is_out_of_range_with_location typ location ->
        write_is_out_of_range_type typ <-> "at" <-> write_location location <-> "is out of range."
      Is_out_of_range_without_location typ -> write_is_out_of_range_type typ <-> "is out of range."
      MIDI_instrument_code_is_out_of_range_command instrument ->
        "MIDI instrument code" <-> show instrument <-> "in the command is out of range."
      MIDI_instrument_code_is_out_of_range_data instrument ->
        "MIDI instrument code" <-> show instrument <-> "is out of range."
      MIDI_instrument_code_is_out_of_range_score instrument location ->
        "MIDI instrument code" <-> show instrument <-> "in the score at" <-> write_location location <-> "is out of range."
      Non_positive_note_length -> "Non-positive note length."
      Note_is_out_of_range_for_MIDI note -> "Note" <-> write_note note <-> "is out of range for MIDI."
      Note_length_denominator_contains_factors_other_than_2_and_3 ->
        "Note length denominator contains factors other than 2 and 3."
      Parse_error_command -> "Parse error in the command."
      Parse_error_playability location -> "Parse error in the playability file at" <-> write_location location <> "."
      Parse_error_score location -> "Parse error in the score at" <-> write_location location <> "."
      Track_ends_with_an_incomplete_triplet -> "Track ends with an incomplete triplet."
      Track_length_mismatch -> "Track length mismatch."
      Velocity_out_of_range location -> "Velocity out of range at " <> write_location location <> "."
  write_is_out_of_range_type :: Is_out_of_range_type -> String
  write_is_out_of_range_type typ =
    case typ of
      Note_length_denominator_IOOR -> "Note length denominator"
      Tempo_IOOR -> "Tempo"
      The_least_common_denominator_of_note_lengths_IOOR -> "The least common denominator of note lengths"
      The_number_of_pitched_tracks_IOOR -> "The number of pitched tracks"
      The_number_of_tracks_IOOR -> "The number of tracks"
      Track_length_IOOR -> "Track length"
      Velocity_IOOR -> "Velocity"
  write_maybe_location :: Maybe Location -> String
  write_maybe_location maybe_location =
    case maybe_location of
      Nothing -> "in the command"
      Just location -> "at" <-> write_location location