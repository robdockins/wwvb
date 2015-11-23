{-# LANGUAGE LambdaCase #-}

module Time.WWVB where

import           Data.Bits
import           Data.Word
import           Data.ByteString( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import           Text.Parsec.Prim
import           Text.Parsec.Pos
import           Text.Parsec.Combinator

data Sign = POS | NEG
 deriving (Eq, Show)

data DSTStatus
  = DSTInEffect
  | DSTNotInEffect
  | DSTStartsToday
  | DSTEndsToday
 deriving (Eq, Show)

data Timecode
  = Timecode
    { wwvbMinute :: Int     -- ^ Minuite in the hour [0,59]
    , wwvbHour   :: Int     -- ^ Hour in day [0,23]
    , wwvbDayOfYear :: Int  -- ^ Day in year [1,366]
    , wwvbDUT1Sign :: Sign  -- ^ Positive of negative DUT1 offset
    , wwvbDUT1Value :: Int  -- ^ DUT1 offset in [0, 9], increments of 0.1 second
    , wwvbYear :: Int       -- ^ Two digit year [0,99]
    , wwvbLeapYear :: Bool  -- ^ Is this a leap year?
    , wwvbLeapSecond :: Bool -- ^ Will there be a leap second at the end of the month?
    , wwvbDSTStatus :: DSTStatus
    }
 deriving (Eq, Show)

data Signal = Marker | Bit Bool
 deriving (Eq)

instance Show Signal where
  show Marker = "m"
  show (Bit False) = "0"
  show (Bit True) = "1"

type CodeFunc = Int -> ([Signal], Int)

codeValue :: Int -> CodeFunc
codeValue m x
    | x >= m    = ([Bit True], x - m)
    | otherwise = ([Bit False], x)

emit :: Signal -> CodeFunc
emit s x = ([s],x)

(<#>) :: CodeFunc -> CodeFunc -> CodeFunc
f <#> g = \x ->
   let (s1, y) = f x
       (s2, z) = g y
    in (s1++s2, z)

code :: Int -> CodeFunc -> [Signal]
code x f = fst $ f x

timecodeToSamples :: Timecode -> ByteString
timecodeToSamples =
  BS.pack . concatMap signalToSamples . timecodeToSignals

signalToSamples :: Signal -> [Word8]
signalToSamples Marker      = [0xFF,0xFF,0xFF,0xFF,0x00]
signalToSamples (Bit True)  = [0xFF,0xFF,0x0F,0x00,0x00]
signalToSamples (Bit False) = [0xFF,0x00,0x00,0x00,0x00]


signalsToTimecode :: [Signal] -> Timecode
signalsToTimecode ss =
    case parse tcParser "" ss of
      Left msg -> error $ show msg
      Right x  -> x

type P = Parsec [Signal] ()

tcParser :: P Timecode
tcParser = do
  marker
  m <- minParser
  marker
  h <- hourParser
  marker
  d <- dayParser
  bit0
  bit0
  sgn <- dut1SignParser
  marker
  val <- dut1ValueParser
  bit0
  yr  <- yearParser
  bit0
  lpyr  <- bitSignal
  lpsec <- bitSignal
  dst   <- dstStatusParser
  marker
  return (Timecode m h d sgn val yr lpyr lpsec dst)

dstStatusParser :: P DSTStatus
dstStatusParser = do
  b1 <- bitSignal
  b2 <- bitSignal
  return $
    case (b2,b1) of
      (False,False) -> DSTNotInEffect
      (False, True) -> DSTEndsToday
      (True ,False) -> DSTStartsToday
      (True , True) -> DSTInEffect

yearParser :: P Int
yearParser = sum <$> sequence
  [ bitValue 80
  , bitValue 40
  , bitValue 20
  , bitValue 10
  , marker >> return 0
  , bitValue 8
  , bitValue 4
  , bitValue 2
  , bitValue 1
  ]

dut1SignParser :: P Sign
dut1SignParser = choice
  [ bit1 >> bit0 >> bit1 >> return POS
  , bit0 >> bit1 >> bit0 >> return NEG
  ]

dut1ValueParser :: P Int
dut1ValueParser = sum <$> sequence
  [ bitValue 8
  , bitValue 4
  , bitValue 2
  , bitValue 1
  ]

minParser :: P Int
minParser = sum <$> sequence
  [ bitValue 40
  , bitValue 20
  , bitValue 10
  , bit0 >> return 0
  , bitValue 8
  , bitValue 4
  , bitValue 2
  , bitValue 1
  ]

hourParser :: P Int
hourParser = sum <$> sequence
  [ bit0 >> return 0
  , bit0 >> return 0
  , bitValue 20
  , bitValue 10
  , bit0 >> return 0
  , bitValue 8
  , bitValue 4
  , bitValue 2
  , bitValue 1
  ]

dayParser :: P Int
dayParser = sum <$> sequence
  [ bit0 >> return 0
  , bit0 >> return 0
  , bitValue 200
  , bitValue 100
  , bit0 >> return 0
  , bitValue 80
  , bitValue 40
  , bitValue 20
  , bitValue 10
  , marker >> return 0
  , bitValue 8
  , bitValue 4
  , bitValue 2
  , bitValue 1
  ]


signal :: Signal -> P ()
signal s = tokenPrim show (\s _ _ -> incSourceColumn s 1)
             (\x -> if x == s then Just () else Nothing)

marker :: P ()
marker = signal Marker

bit0 :: P ()
bit0 = signal $ Bit False

bit1 :: P ()
bit1 = signal $ Bit True

anySignal :: P Signal
anySignal = tokenPrim show (\s _ _ -> incSourceColumn s 1) Just

bitValue :: Int -> P Int
bitValue n = do
  b <- bitSignal
  return $! if b then n else 0

bitSignal :: P Bool
bitSignal = tokenPrim show (\s _ _ -> incSourceColumn s 1)
           (\case Bit b -> Just b
                  _     -> Nothing)

timecodeToSignals :: Timecode -> [Signal]
timecodeToSignals tc = concat
 [ [Marker]                         -- bit  0
 , codeMinutes (wwvbMinute tc)      -- bits 1-8
 , [Marker]                         -- bit  9
 , codeHour (wwvbHour tc)           -- bits 10-18
 , [Marker]                         -- bit  19
 , codeDay (wwvbDayOfYear tc)       -- bits 20-33
 , [Bit False, Bit False]           -- bits 34-35
 , codeDUT1Sign (wwvbDUT1Sign tc)   -- bits 36-38
 , [Marker]                         -- bit  39
 , codeDUT1Value (wwvbDUT1Value tc) -- bits 40-43
 , [Bit False]                      -- bit  44
 , codeYear (wwvbYear tc)           -- bits 45-53
 , [Bit False]                      -- bit  54
 , [Bit (wwvbLeapYear tc)]          -- bit  55
 , [Bit (wwvbLeapSecond tc)]        -- bit  56
 , codeDSTStatus (wwvbDSTStatus tc) -- bits 57-58
 , [Marker]                         -- bit  59
 ]

 where codeMinutes :: Int -> [Signal]
       codeMinutes x
        | 0 <= x && x <= 59 = code x $
            codeValue 40 <#>
            codeValue 20 <#>
            codeValue 10 <#>
            emit (Bit False) <#>
            codeValue 8 <#>
            codeValue 4 <#>
            codeValue 2 <#>
            codeValue 1
        | otherwise = error $ "illegal minute value: " ++ show x

       codeHour x
        | 0 <= x && x <= 23 = code x $
            emit (Bit False) <#>
            emit (Bit False) <#>
            codeValue 20 <#>
            codeValue 10 <#>
            emit (Bit False) <#>
            codeValue 8 <#>
            codeValue 4 <#>
            codeValue 2 <#>
            codeValue 1
        | otherwise = error $ "illegal hour value: " ++ show x

       codeDay x
        | 1 <= x && x <= 366 = code x $
            emit (Bit False) <#>
            emit (Bit False) <#>
            codeValue 200 <#>
            codeValue 100 <#>
            emit (Bit False) <#>
            codeValue 80 <#>
            codeValue 40 <#>
            codeValue 20 <#>
            codeValue 10 <#>
            emit Marker <#>
            codeValue 8 <#>
            codeValue 4 <#>
            codeValue 2 <#>
            codeValue 1
        | otherwise = error $ "illegal day value: " ++ show x

       codeDUT1Sign POS = map Bit [True,False,True]
       codeDUT1Sign NEG = map Bit [False,True,False]

       codeDUT1Value x
        | 0 <= x && x <= 9 = code x $
            codeValue 8 <#>
            codeValue 4 <#>
            codeValue 2 <#>
            codeValue 1
        | otherwise = error $ "illegal DUT1 value: " ++ show x

       codeYear x
        | 0 <= x && x <= 99 = code x $
            codeValue 80 <#>
            codeValue 40 <#>
            codeValue 20 <#>
            codeValue 10 <#>
            emit Marker <#>
            codeValue 8 <#>
            codeValue 4 <#>
            codeValue 2 <#>
            codeValue 1
        | otherwise = error $ "illegal year value: " ++ show x

       codeDSTStatus DSTInEffect    = map Bit [True, True]
       codeDSTStatus DSTNotInEffect = map Bit [False, False]
       codeDSTStatue DSTStartsToday = map Bit [True, False]
       codeDSTStatue DSTEndsToday   = map Bit [False, True]

splitInto :: Int -> [a] -> [[a]]
splitInto n [] = []
splitInto n xs = h : splitInto n t
  where (h,t) = splitAt n xs

showSamples :: ByteString -> String
showSamples = showRawSamples . Base64.decodeLenient

showRawSamples :: ByteString -> String
showRawSamples bs = unlines $ splitInto 40 $ BS.foldl f id bs []
  where f ss b = ss . g b
        g b x = [ if testBit b i then 'x' else '_'
                | i <- reverse [0..7]
                ] ++ x
