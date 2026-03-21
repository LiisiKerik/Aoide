module Main (main) where --
  import Composition.Errors --
  import Composition.Lilypond --
  import Composition.MIDI --
  import Composition.Keyboard --
  import Composition.Parser --
  import Composition.Score --
  import Composition.Write --
  import Control.Monad.Except --
  import Control.Monad.Trans.Except --
  import Data.Char --
  import Data.Functor --
  import Data.List --
  import Data.Maybe --
  import Parser.Files --
  import Parser.Locations --
  import Parser.Parser --
  import Parser.Utilities --
  import System.Environment --
  data Char_class = --
    Delimiter_char Token | --
    File_path_char Char | --
    Invalid_char | --
    Keyword_char Char | --
    Nonzero_char Char | --
    Quote_char | --
    Text_char Char | --
    Whitespace_char | --
    Zero_char --
  data Command = --
    Keyboard_command { --
      playable :: File_path, --
      source :: File_path, --
      header_instrument :: Maybe String, --
      midi_instrument :: MIDI_instrument, --
      destination :: File_path} | --
    Lilypond {source :: File_path, destination :: File_path} | --
    MIDI {source :: File_path, destination :: File_path} --
  type Parser = Parser' Token Error --
  data Token = --
    Destination_token | --
    Eq_token | --
    File_path_token String | --
    Instrument_name_token | --
    Keyboard_token | --
    Left_curly_bracket_token | --
    Lilypond_token | --
    MIDI_instrument_token | --
    MIDI_token | --
    Nat_token Integer | --
    Playable_token | --
    Right_curly_bracket_token | --
    Source_token | --
    Text_token String --
  type Tokeniser = Tokeniser' Char_class Token Error --
  deriving instance Eq Char_class --
  deriving instance Eq Token --
  deriving instance Show Char_class --
  deriving instance Show Command --
  deriving instance Show Token --
  classify_char :: Char -> Char_class --
  classify_char c = --
    case c of --
      ' ' -> Whitespace_char --
      _ | elem c "!#$%&'()*+-,:;<>?@[]^`|~" -> Text_char c --
      '"' -> Quote_char --
      _ | elem c "'_" || isLetter c -> Keyword_char c --
      _ | elem c "./" -> File_path_char c --
      '0' -> Zero_char --
      _ | isDigit c && c /= '0' -> Nonzero_char c --
      '=' -> Delimiter_char Eq_token --
      '{' -> Delimiter_char Left_curly_bracket_token --
      '}' -> Delimiter_char Right_curly_bracket_token --
      _ -> Invalid_char --
  construct_keyword :: String -> Either (Location -> Error) Token --
  construct_keyword keyword = --
    case lookup keyword keywords of --
      Nothing -> Left (\ _ -> Invalid_keyword keyword) --
      Just token -> Right token --
  convert_midi_instrument :: Integer -> Either (Location -> Error) MIDI_instrument --
  convert_midi_instrument midi_instrument = --
    do --
      check (Is_out_of_range_with_location MIDI_instrument_code_IOOR) (between 0 127 midi_instrument) --
      Right (fromIntegral midi_instrument) --
  delimiter_char :: Char_class -> Maybe Token --
  delimiter_char char_class = --
    case char_class of --
      Delimiter_char token -> Just token --
      _ -> Nothing --
  file_path_char :: Char_class -> Maybe Char --
  file_path_char char_class = --
    case char_class of --
      File_path_char c -> Just c --
      Keyword_char c -> Just c --
      Nonzero_char c -> Just c --
      Zero_char -> Just '0' --
      _ -> Nothing --
  file_path_token :: Token -> Maybe String --
  file_path_token token = --
    case token of --
      File_path_token file_path -> Just file_path --
      _ -> Nothing --
  keyword_char :: Char_class -> Maybe Char --
  keyword_char char_class = --
    case char_class of --
      Keyword_char c -> Just c --
      _ -> Nothing --
  keywords :: [(String, Token)] --
  keywords = --
    [ --
      ("Keyboard", Keyboard_token), --
      ("Lilypond", Lilypond_token), --
      ("MIDI", MIDI_token), --
      ("destination", Destination_token), --
      ("instrument_name", Instrument_name_token), --
      ("midi_instrument", MIDI_instrument_token), --
      ("playable", Playable_token), --
      ("source", Source_token)] --
  main :: IO () --
  main = --
    do --
      args <- getArgs --
      result <- runExceptT (main' (intercalate " " args)) --
      case result of --
        Left err -> putStrLn (write_error err) --
        Right () -> return () --
  main' :: String -> ExceptT Error IO () --
  main' text = --
    do --
      command <- except (Main.parse text) --
      case command of --
        Keyboard_command {playable = playable_file, source, header_instrument, midi_instrument, destination} -> --
          do --
            playable <- parse_playable playable_file --
            original_score <- Composition.Parser.parse source --
            keyboard_score <- except (keyboard playable header_instrument midi_instrument original_score) --
            write destination keyboard_score --
        Lilypond {source, destination} -> --
          do --
            score <- Composition.Parser.parse source --
            lilypond destination score --
        MIDI {source, destination} -> --
          do --
            score <- Composition.Parser.parse source --
            midi score destination --
      return () --
  nat_char :: Char_class -> Maybe Char --
  nat_char char_class = --
    case char_class of --
      Zero_char -> Just '0' --
      Nonzero_char c -> Just c --
      _ -> Nothing --
  nat_token :: Token -> Maybe Integer --
  nat_token token = --
    case token of --
      Nat_token i -> Just i --
      _ -> Nothing --
  nonzero_char :: Char_class -> Maybe Char --
  nonzero_char char_class = --
    case char_class of --
      Nonzero_char c -> Just c --
      _ -> Nothing --
  parse :: String -> Either Error Command --
  parse command = fromJust (parse' classify_char (return next_char) tokenise parse_command Parse_error command) --
  parse_command :: Parser Command --
  parse_command = parse_keyboard <+> parse_lilypond <+> parse_midi --
  parse_curly_brackets :: Parser t -> Parser t --
  parse_curly_brackets = parse_brackets Left_curly_bracket_token Right_curly_bracket_token --
  parse_eq :: Parser () --
  parse_eq = parse_token Eq_token --
  parse_field :: Token -> Parser t -> Parser t --
  parse_field name parse_t = --
    do --
      parse_token name --
      parse_eq --
      parse_t --
  parse_file_path :: String -> Parser File_path --
  parse_file_path ext = fmap_filter_parser (parse_file_path' (\ err _ -> File_error err) ext) (parse_token' file_path_token) --
  parse_keyboard :: Parser Command --
  parse_keyboard = --
    parse_struct --
      Keyboard_token --
      (do --
        playable <- parse_field Playable_token (parse_file_path "key") --
        source <- parse_field Source_token (parse_file_path "aoi") --
        header_instrument <- parse_optional_field Instrument_name_token parse_text --
        midi_instrument <- parse_field MIDI_instrument_token parse_midi_instrument --
        destination <- parse_field Destination_token (parse_file_path "aoi") --
        return (Keyboard_command {playable, source, header_instrument, midi_instrument, destination})) --
  parse_lilypond :: Parser Command --
  parse_lilypond = --
    parse_struct --
      Lilypond_token --
      (do --
        source <- parse_field Source_token (parse_file_path "aoi") --
        destination <- parse_field Destination_token (parse_file_path "ly") --
        return (Lilypond {source, destination})) --
  parse_midi :: Parser Command --
  parse_midi = --
    parse_struct --
      MIDI_token --
      (do --
        source <- parse_field Source_token (parse_file_path "aoi") --
        destination <- parse_field Destination_token (parse_file_path "mid") --
        return (MIDI {source, destination})) --
  parse_midi_instrument :: Parser MIDI_instrument --
  parse_midi_instrument = fmap_filter_parser convert_midi_instrument parse_nat --
  parse_nat :: Parser Integer --
  parse_nat = parse_token' nat_token --
  parse_optional_field :: Token -> Parser t -> Parser (Maybe t) --
  parse_optional_field name parse_t = parse_default Nothing (Just <$> parse_field name parse_t) --
  parse_struct :: Token -> Parser Command -> Parser Command --
  parse_struct name parse_fields' = --
    do --
      parse_token name --
      parse_curly_brackets parse_fields' --
  parse_text :: Parser String --
  parse_text = parse_token' text_token --
  text_char :: Char_class -> Maybe Char --
  text_char char_class = --
    case char_class of --
      Delimiter_char token -> --
        Just --
          (case token of --
            Eq_token -> '=' --
            Left_curly_bracket_token -> '{' --
            Right_curly_bracket_token -> '}' --
            _ -> undefined) --
      File_path_char c -> Just c --
      Invalid_char -> Nothing --
      Keyword_char c -> Just c --
      Nonzero_char c -> Just c --
      Quote_char -> Nothing --
      Text_char c -> Just c --
      Whitespace_char -> Just ' ' --
      Zero_char -> Just '0' --
  text_token :: Token -> Maybe String --
  text_token token = --
    case token of --
      Text_token text -> Just text --
      _ -> Nothing --
  tokenise :: Tokeniser () --
  tokenise = void (parse_many tokenise_1) --
  tokenise_1 :: Tokeniser () --
  tokenise_1 = --
    tokenise_delimiter <+> ((tokenise_nat <+> tokenise_keyword) <+ tokenise_file_path) <+> tokenise_text <+> tokenise_whitespace --
  tokenise_delimiter :: Tokeniser () --
  tokenise_delimiter = add_token (parse_token' delimiter_char) --
  tokenise_file_path :: Tokeniser () --
  tokenise_file_path = add_token (File_path_token <$> parse_some (parse_token' file_path_char)) --
  tokenise_keyword :: Tokeniser () --
  tokenise_keyword = add_token (fmap_filter_parser construct_keyword (parse_some (parse_token' keyword_char))) --
  tokenise_nat :: Tokeniser () --
  tokenise_nat = add_token (Nat_token <$> (tokenise_zero <+> tokenise_positive_int)) --
  tokenise_positive_int :: Tokeniser Integer --
  tokenise_positive_int = read <$> ((:) <$> parse_token' nonzero_char <*> parse_many (parse_token' nat_char)) --
  tokenise_quotes :: Tokeniser t -> Tokeniser t --
  tokenise_quotes = parse_brackets Quote_char Quote_char --
  tokenise_text :: Tokeniser () --
  tokenise_text = add_token (Text_token <$> tokenise_quotes (parse_many (parse_token' text_char))) --
  tokenise_whitespace :: Tokeniser () --
  tokenise_whitespace = parse_token Whitespace_char --
  tokenise_zero :: Tokeniser Integer --
  tokenise_zero = --
    do --
      parse_token Zero_char --
      return 0 --