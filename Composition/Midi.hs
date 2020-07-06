--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NegativeLiterals, StandaloneDeriving #-}
{-|
Description: A module for generating MIDI files.

This module contains the data structures and the function for generating MIDI files from note sequences.
-}
module Composition.Midi (Instrument (..), Track (..), Part (..), midi) where
  import Composition.Notes (
    Note (..),
    Note_name (..),
    Rat,
    Simultaneous (..),
    Time (..),
    Time_and_position (..),
    sequential_length)
  import Composition.Theory (semitones_from_c)
  import Control.Monad (join, zipWithM)
  import Data.ByteString (pack, writeFile)
  import Data.Foldable (traverse_)
  import Data.List (delete, transpose)
  import Data.Ratio ((%), numerator, denominator)
  import Data.Word (Word8)
  type Err = Either String
  -- | MIDI instruments.
  data Instrument =
    Accordion |
    Bassoon |
    Bells |
    Cello |
    Clarinet |
    Double_bass |
    Dulcimer |
    English_horn |
    Flute |
    French_horn |
    Glockenspiel |
    Guitar |
    Harp |
    Harpsichord |
    Oboe |
    Organ |
    Piano |
    Piccolo |
    Pizzicato_strings |
    Recorder |
    Timpani |
    Trombone |
    Trumpet |
    Viola |
    Violin |
    Voice
  -- | Each part can have a different time signature, tempo and instrumentation. The second argument is the tempo specified in
  -- beats per minute.
  data Part = Part Time_and_position Int [Track]
  -- | A track consists of one homorhythmic note sequence. Heterorhythmic voices require separate tracks.
  data Track = Track Instrument [Simultaneous]
  deriving instance Show Instrument
  deriving instance Show Part
  deriving instance Show Track
  channels :: [Word8]
  channels = delete 9 [0 .. 15]
  check :: String -> Bool -> Err ()
  check err condition =
    case condition of
      False -> Left err
      True -> Right ()
  check_range :: Ord t => String -> t -> t -> t -> Err ()
  check_range typ min_t max_t x = check (typ ++ " out of range.") (min_t <= x && max_t >= x)
  encode_chunk :: [Word8] -> [Word8] -> [Word8]
  encode_chunk typ dat = typ ++ encode_int_fixed 4 (length dat) ++ dat
  encode_end_track :: [Word8]
  encode_end_track = encode_meta_event_0 47 []
  encode_event :: Int -> Rat -> [Word8] -> [Word8]
  encode_event lcd time dat = encode_time lcd time ++ dat
  encode_format :: [Word8]
  encode_format = [0, 1]
  encode_header :: Int -> Int -> Err [Word8]
  encode_header number_of_tracks lcd =
    do
      let quarter = length_in_ticks lcd (1 % 4)
      check_range "The number of ticks in quarter note" 1 max_ticks_in_quarter_note quarter
      Right
        (encode_chunk
          [77, 84, 104, 100]
          (encode_format ++ encode_int_fixed 2 (1 + number_of_tracks) ++ encode_int_fixed 2 quarter))
  encode_instrument :: Word8 -> Instrument -> [Word8]
  encode_instrument channel instrument = encode_midi_event channel 12 [encode_instrument' instrument]
  encode_instrument' :: Instrument -> Word8
  encode_instrument' instrument =
    case instrument of
      Accordion -> 21
      Bassoon -> 70
      Bells -> 14
      Cello -> 42
      Clarinet -> 71
      Double_bass -> 43
      Dulcimer -> 15
      English_horn -> 69
      Flute -> 73
      French_horn -> 60
      Glockenspiel -> 9
      Guitar -> 24
      Harp -> 46
      Harpsichord -> 6
      Oboe -> 68
      Organ -> 19
      Piano -> 0
      Piccolo -> 72
      Pizzicato_strings -> 45
      Recorder -> 74
      Timpani -> 47
      Trombone -> 57
      Trumpet -> 56
      Viola -> 41
      Violin -> 40
      Voice -> 52
  encode_int_fixed :: Integer -> Int -> [Word8]
  encode_int_fixed n i =
    case n of
      0 -> []
      _ -> encode_int_fixed (n - 1) (div i 256) ++ [fromIntegral i]
  encode_int_flexible :: Int -> [Word8]
  encode_int_flexible i = encode_int_flexible' (div i 128) ++ [fromIntegral (mod i 128)]
  encode_int_flexible' :: Int -> [Word8]
  encode_int_flexible' i =
    case i of
      0 -> []
      _ -> encode_int_flexible' (div i 128) ++ [128 + fromIntegral (mod i 128)]
  max_number_of_tracks :: Int
  max_number_of_tracks = length channels
  max_ticks_in_quarter_note :: Int
  max_ticks_in_quarter_note = 2 ^ (16 :: Integer) - 1
  max_track_length :: Int
  max_track_length = 2 ^ (32 :: Integer) - 1
  -- | Encodes the score in MIDI format and writes it to the specified file.
  midi :: String -> [Part] -> IO ()
  midi file_name score =
    case encode_score score of
      Left err -> putStrLn ("Midi error. " ++ err)
      Right encoding -> Data.ByteString.writeFile (file_name ++ ".mid") (pack encoding)
  encode_meta_event :: Int -> Rat -> Word8 -> [Word8] -> [Word8]
  encode_meta_event lcd time typ dat = encode_event lcd time ([255, typ, fromIntegral (length dat)] ++ dat)
  encode_meta_event_0 :: Word8 -> [Word8] -> [Word8]
  encode_meta_event_0 = encode_meta_event 1 0
  encode_midi_event :: Word8 -> Word8 -> [Word8] -> [Word8]
  encode_midi_event channel typ dat = encode_event 1 0 ([channel + 16 * typ] ++ dat)
  encode_note :: Note -> Word8
  encode_note (Note octave note_name) = fromIntegral (12 * (1 + octave) + semitones_from_c note_name)
  encode_note_off :: Word8 -> Note -> [Word8]
  encode_note_off channel note = encode_midi_event channel 8 [encode_note note, velocity]
  encode_note_on :: Word8 -> Note -> [Word8]
  encode_note_on channel note = encode_midi_event channel 9 [encode_note note, velocity]
  encode_part :: Int -> Int -> Part -> Err [[Word8]]
  encode_part number_of_tracks lcd (Part (Time_and_position time _) tempo tracks) =
    do
      tempo' <- encode_tempo time tempo
      let tracks' = (\(Track instrument sequential) -> Track instrument (sequential ++ [Simultaneous [] 1])) <$> tracks
      len <-
        case track_length <$> tracks' of
          [] -> Right 1
          len' : lengths ->
            do
              check "Track length mismatch." (all ((==) len') lengths)
              Right len'
      check_range "Part length" 0 max_note_length (length_in_ticks lcd len)
      let rest = encode_rest lcd len
      tracks'' <- zipWithM (encode_track lcd) channels tracks'
      Right ([tempo' ++ rest] ++ tracks'' ++ replicate (number_of_tracks - length tracks') rest)
  encode_rest :: Int -> Rat -> [Word8]
  encode_rest lcd len = encode_text lcd len []
  encode_score :: [Part] -> Err [Word8]
  encode_score parts =
    do
      let parts' = parts ++ [Part (Time_and_position (Time [2, 2] 4) 0) 100 []]
      let number_of_tracks = maximum (number_of_tracks_in_part <$> parts')
      check_range "Number of tracks" 0 max_number_of_tracks number_of_tracks
      let lcd = lcm_all (lcd_of_part <$> parts')
      header <- encode_header number_of_tracks lcd
      parts'' <- traverse (encode_part number_of_tracks lcd) parts'
      tracks <- traverse encode_tracks (transpose parts'')
      Right (header ++ join tracks)
  encode_sequential :: Int -> Word8 -> [Simultaneous] -> Err [Word8]
  encode_sequential lcd channel sequential = join <$> traverse (encode_simultaneous lcd channel) sequential
  encode_simultaneous :: Int -> Word8 -> Simultaneous -> Err [Word8]
  encode_simultaneous lcd channel (Simultaneous notes len) =
    do
      traverse_ (check_range "Note" min_note max_note) notes
      check "Non-positive note length." (0 < len)
      Right ((notes >>= encode_note_on channel) ++ encode_rest lcd len ++ (notes >>= encode_note_off channel))
  encode_tempo :: Time -> Int -> Err [Word8]
  encode_tempo time tempo =
    do
      let tempo' = microseconds_in_quarter_note time tempo
      check_range "Tempo" 1 max_tempo tempo'
      Right (encode_meta_event_0 81 (encode_int_fixed 3 tempo'))
  encode_text :: Int -> Rat -> [Word8] -> [Word8]
  encode_text lcd time = encode_meta_event lcd time 1
  encode_time :: Int -> Rat -> [Word8]
  encode_time lcd time = encode_int_flexible (length_in_ticks lcd time)
  encode_track :: Int -> Word8 -> Track -> Err [Word8]
  encode_track lcd channel (Track instrument sequential) =
    do
      sequential' <- encode_sequential lcd channel sequential
      Right (encode_instrument channel instrument ++ sequential')
  encode_tracks :: [[Word8]] -> Err [Word8]
  encode_tracks tracks =
    do
      let track = join tracks ++ encode_end_track
      check_range "Track length" 0 max_track_length (length track)
      Right (encode_chunk [77, 84, 114, 107] track)
  lcd_of_part :: Part -> Int
  lcd_of_part (Part time_and_initial_position _ tracks) =
    lcm (lcd_of_time_and_position time_and_initial_position) (lcm_all (lcd_of_track <$> tracks))
  lcd_of_sequential :: [Simultaneous] -> Int
  lcd_of_sequential sequential = lcm_all (lcd_of_simultaneous <$> sequential)
  lcd_of_simultaneous :: Simultaneous -> Int
  lcd_of_simultaneous (Simultaneous _ len) = denominator len
  lcd_of_time_and_position :: Time_and_position -> Int
  lcd_of_time_and_position (Time_and_position time position) = lcm (time_denominator time) (denominator position)
  lcd_of_track :: Track -> Int
  lcd_of_track (Track _ sequential) = lcd_of_sequential sequential
  lcm_all :: Integral t => [t] -> t
  lcm_all = foldr lcm 1
  length_in_ticks :: Int -> Rat -> Int
  length_in_ticks lcd time = numerator time * div lcd (denominator time)
  max_note :: Note
  max_note = Note 9 G
  max_note_length :: Int
  max_note_length = 2 ^ (32 :: Integer) - 1
  max_tempo :: Int
  max_tempo = 2 ^ (24 :: Integer) - 1
  microseconds_in_minute :: Int
  microseconds_in_minute = 60000000
  microseconds_in_quarter_note :: Time -> Int -> Int
  microseconds_in_quarter_note (Time _ den) tempo = round (microseconds_in_minute * den % (4 * tempo))
  min_note :: Note
  min_note = Note -2 B_sharp
  number_of_tracks_in_part :: Part -> Int
  number_of_tracks_in_part (Part _ _ tracks) = length tracks
  time_denominator :: Time -> Int
  time_denominator (Time _ den) = den
  track_length :: Track -> Rat
  track_length (Track _ sequential) = sequential_length sequential
  velocity :: Word8
  velocity = 127
--------------------------------------------------------------------------------------------------------------------------------