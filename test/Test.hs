{-# LANGUAGE OverloadedStrings #-}

import           Data.Bits
import           Data.ByteString( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.List
import           Time.WWVB

ss :: BS.ByteString
-- ss = "/////gD/AAAAAf//8AAB/8AOAAD/gAAAAf+AAA+A///4AAH/gAAAH/8AAAAB/////wH/gAAAAf8BwAAA/4AAAAH/gAAAAf/ewAAB/8AAAAH///wAAf//8AAB///4AAH////+AP8AAAAB//wABwH///AAH////AAB//8AAD//4AQAA/8AAAAB///gAAH/gAAAAf////8A/8AAAAP///wAD///+AAB/4AAAAH/gAAAA/+AAAHD///wAAD/AAAAA///8AAB/////wD/wAAAAf/gAAAB///gAfH/gAAAAf+AAAAB/8AAAAH/gAAAAf/wH/AD///4AAD////+Af8AAAAB///4AAH/+AAAAf//4AAA/4AAAAH/8AAAD/4AAAAA/4AAAAP/AAAcAf////8B"

-- ss = "/////wD/wAAAAH//8AAA/8AAAAD/wAAAAf/AAAAA///wAAB/4AAAAH//8AAA/////wD/gAAAAf+AAAAAf4AAAAB/4AAAAP/4AAAA/4AAAAB///AAAf///AAAf//wAAD/////gP8AAAAA/8AAAAD///gAAP//8AAA/4AAAAB/4AAAAP/AAAAA///4AAB/4AAAAP////+A/8AAAAD///AAAf//+AAA//AAAAD/wAAAAH/AAAAA///4AAD/wAAAAP///gAA/////wB/gAAAAP+AAAAA///4AAD/gAAAAP/AAAAA/+AAAAD/wAAAAP8AAAAA///wAAD/////gP+AAAAA///4AAD//4AAAP///gAA/4AAAAD/wAAAAP/gAAAA//AAAAD/AAAAAP////+A"

-- ss = "/////wD/gAAAAP///AAA/8AAAAD/gAAAAP+AAAAA///wAAD///wAAP/wAAAA/////wD/wAAAAP+AAAAA/+AAAAD/4AAAAP+AAAAA/8AAAAD///AAAP//8AAA///4AAD/////AP+AAAAA/4AAAAH///gAAf//+AAA/4AAAAD/gAAAAP+AAAAA///4AAB/8AAAAP////4A/wAAAAD///AAAP//8AAA//gAAAD/gAAAAf/gAAAA///wAAH/4AAAAP//8AAA/////wD/+gAAAP+AAAAB///+AAH/gAAAAf8AAAAB/4AAAAH/4AAAAP/4AAAA///4AAD/////AP/AAAAA///wAAD/gAAAAf//8AAA/4AAAAH/4AAAAP/QA/AA/4AAAAD/wAAAAf////+A"

--ss = "/////gB/wAAAAP//+AAA/4AAAAD/+AAAAP/4AAAA///4AAB///AAAP//+AAA/////4D/wAAAAH/gAAAA//gADAD/AAAAAP/+AAAB//gAAAD///gAAP//8AAH///4AAD/////4P+AAAAA/+AAAAD///AAAP///AAA/8AAAAB/wAAAPf+AAAAB///8AAD//gAAAf////+AfwAAAAD///gAAf//+AAA/8AAAAD/gAAAAP+AAAAAf//wAAD/gAAAAP//+AAA/////wD/4AAAAH/gAAAA///8AAD/wAAAAH+AAAAAfwAAAAD/gAAAAP/gAAAA///4AAH/////AP/wAAAA///wAAD/AAAAAP///wAB/+AAAAB/gAAAAf//gA+A/4AAAAH/gAAAAP/////A"

--ss = "/////wD/wAAAAH///AAA///8AAB/AAAAAP+AAAAA/8AAAAH/wAAAAP//+AAA/////wB/AAAAAP/AAAAAf/wAAPn/gAAAAP/AAAAA//gAAAD///+AAP///AAA///8AAB////+AP//AAAB/4AAAAD///wAAP//+AAAf/AAAAD/+AAAAP+AAAAA///wAAH//gAAH/////8A/8AAAAD///wAAf//8AAA///gAAH/wAAAAP/wAAAAf//wAAH/gAA/wP//8AAA/////4B/AAAAAH/AAAAA///wAAH/wAAAAP/wAAAA/8AAAAD/4AAAAf/AAAAB///8AAD/////AP/AAAAB///4AAD/4AAAAP//+AAB/8AAAAD/4AAAAP/YAAAB//gAAAB/wAPAAf////8A"

-- ss = "/////wH/wAAAAf//6AAB///4AAH/gAAAAf/AAAAB/+AAAB////AAAf8AAAAD/////gH/gAAAAf/wAAAB/wAAAAH/wAAAA//AAAAB//gHAAD///AAAf///AAA///wAB/////+Af8AAAf//gAAAAH///AAAP//8AAB/4AAAAD/wAAAH/8AHgAA///gAAH/AAAAAP////4B//AAAAH///gAA///8AAB/gAD/8H/gAAAB/+AAf/x///wAAH//AAAA////gAD/////gD/gAAAAf//gAAB///gAAH/7AAAAP/AAAAB/+AAAAH//AAAAf+AAAAB///4AAP////+AP4AAAAB///wAAH/4AAAAf///gAB/wAAAAB/wAAAAf/sgAAB/4AAAAH/AB4AAP////4B"

-- ss = "/////gD/AAAAAH//8AAA///8A+D/wAAAAP/AAAAAf8AAAAD///gAAf//+AAA/////wD/wAAAAH+AAAAB/8AA8AD/wAAAAP+AAAAA//wAAAD///AAAP//8AAA///wAAD/////Af+AAAAA/8AAAAH///AAAf//+AAB/4AAAAD/4AAAAP/AAAAA///wAAD/wAAAAP////8AP4AAAAD///gAAP//8AAB/8AAAAD/gAAADf/AAAAB///wAAD/gAAAAH//+AAB/////wD/wAAAQH8AAAAB///8AAD/8AAAAP+AAAAA/8AAAAD/AAHwAf+AAAAB//8wAAH/////Af+AAAAB///wAAH/wAH8AH///gAB/4AAAAH/wAAAAP/AAAAA//+AAAf/gAAAP/////8A"

ss = "/////gD/AAAAAf//8AAA///gAAH/gAAAgf//8AAB/wAAOAH/gAGAAP8AAAAH/////wH/gAAAA//wAAAA/8AAAAD/gAAAAP8AAAAA/wAAAAf///BAAf//+AAA///wAAH////+AP8AAAAB/8AAAAD///gAAf//8AAB/sAAAMH/gAAAAP+AAAAB///wAAD+AAAAAf///8+A/8AAAAH///AAAf//8AAB/+AAAAD/wAAAAf/AAA+B///wAAH/gAAAAf//8AAA/////gD/AAAAAf/gAAAB///wAAD/AAABA//AAAAB/8AAAAD/4AAAAP/AAAAB///gAAH////+Af8AAA/5///gAAB/8AAAAP//4AAB/wAAAAD//gAAA/+AAAAD/8AAAD//4AAAAf////4A"

-- ss = "/////gD///AAAP+AAAAAf4AAAAD/gAAAAP/AAAAA/4AAAAD/AAAAAP//+AAA/////wD/wAAAAP/AAAAB/8AAAAH/gAAAAP8AAAAA/4AAAAD///wAAP//+AAA///4AAH/////gP/AAAAA/8AAAAD///gAAf//8AAB/4AAAAD/gAAAAf/8AAAA///wAAD/gAAAAP////8A/4AAAAD///gAAf///gAA/+AAAAD/8AAAAf+AAAAA///4AAD/gAAAAP//8AAB/////gD/gAAAAP/gAAAB///4AAD/4AAAAP/AAAAA//4AAAD/8AAAAP/gAAAB///4AAD////+AP/gAAAA///gAAD/4AAAAH//+AAB/4AAAAD/gAAAAP/AAAAAf8AAAAD/4AAAAP////8A"

--ss = "/////4D///AAAP/gAAAA///4AAD/+AAAAP/8AAAAf//4AAD/wAAAAP///gAA/////4B/AAAAAP/gAAAA/4AAAAD/wAAAAP/AAAAB//4AAAD///AAAP//8AAA///4AAD////+AP/AAAAA/+AAAAD///gAAP///gAA/4AAAAB/gAAAAP/AAAAA///wAAD/gAAAAH////8A/8AAAAD///gAAP//8AAA/+AAAAD/gAAAAP/AAAAA///4AAD/8AAAAH///AAA/////4A/gAAAAf/AAAAA///8AAD/gAAAAP/wAAAA/4AAAAD/gAAAAP/AAAAA///wAAD/////gP/AB8AA///4AAD/4AAAAP//+AAA/4AAAAD/wAAAAf/4AAAA/8AAAAD/wAAAAf////+A"

-- ss = "/////wB///AAAH8AAAAA///wAAD/wAAAAP/gAAAA///wAAD///AAAP/gAAAA/////wB/8AAAAP/wAAAA/4AAAAD/8AAAAP/AAAAA/8AAAAH///AAAP//+AAA///wAAD/////AP/AAAAA/4AAAAD///gAAP//8AAA/4AAAAD/4AAAAP/gAAAA///wAAD/4AAAAP////+A/wAAAAD///gAAP//8AAA/+AAAAD/AAAAAP/wAAAA///4AAD/gAAAAP//+AAB/////wD/gAAAAP/4AAAA///wAAA/4AAAAP+AAAAA/4AAAAD/8AAAAP/AAAAA///4AAD/////AP+AAAAAf//wAAD/gAAAAP//+AAA/wAAAAD/wAAAAP/wAAAA/8AAAAD/gAAAAf////8A"

-- ss = "/////wB///AAAP+AAAAA///8AAD/4AAAAP//+AAA/8AAAAD/gAAAAP/AAAAA/////4D/4AAAAP/AAAAA/+AAAAD/wAAAAP/gAAAA/8AAAAB//+AAAP///AAAf//4AAD/////AH/AAAAA/+AAAAD///AAAP///AAAf/AAAAD/4AAAAP/AAAAA///wAAB/gAAAAP////8A/4AAAAD///gAAP//+AAA/4AAAAD/gAAAAP/4AAAA///wAAD/wAAAAf//+AAA/////wD/AAAAAP/gAAAA///wAAD/wAAAAP+AAAAA/4AAAAD/wAAAAP/gAAAA///8AAD/////AP/gAAAB///4AAD/wAAAAP//8AAA/8AAAAB/4AAAAP/gAAAA//AAAAD/gAAAAP////8A"

--ss = "/////4D/wAAAAP8AAAAA/wAAAAD/wAAAAP+AAAAA/4AAAAD/wAAAAP/4AAAA/////wD/wAAAAP/gAAAA//4AAAB/gAAAAP/gAAAA///wAAB/4AAAAP/AAAAA/8AAAAB/////gP/gAAAA/8AAAAD///gAAP//+AAA/8AAAAH/4AAAAP+AAAAA///wAAD/gAAAAP////8Af4AAAAD///wAAH//8AAB//AAAAD/4AAAAP/8AAAA///4AAD/wAAAAP///gAA/////wD/gAAAAP+AAAAA///wAAD/wAAAAP/gAAAA/4AAAAD//AAAAP+AAAAAf//wAAB/////AP/gAAAA///8AAD//gAAAP//+AAA/8AAAAD/gAAAAH/AAAAA/4AAAAD/wAAAAf////+A"

--ss = "/////wB/8AAAAP/8AAAAf4AAAAD/wAAAAH+AAAAA/4AAAAD/+AAAAP//+AAA/////gD/wAAAAP/gAAAA/8AAAAD/gAAAAP/AAAAA///4AAH/+AAAAP+AAAAA/8AAAAD/////gH/AAAAA/+AAAAD///wAAP//8AAA/4AAAAB/wAAAAP+AAAAA///8AAD/gAAAAP////8A/+AAAAD///gAAP///AAA//4AAAD/AAAAAP//4AAA///8AAD/4AAAAP///gAA/////wD/gAAAAH+AAAAA///wAAD/+AAAAf/wAAAA/8ADwAD/gAAAAP+AAAAAf//4AAH/////AH8AAAAB///wAAD//AAAAP//+AAA/4AAAAD/wAAAAH/wAAAA/4AAAAD/wAAAAP////8A"

--BS.concat $ replicate 8 $ BS.replicate 50 (fromIntegral (fromEnum '/'))
{-
    BS.concat
  [ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  , "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  ]
-}

-- | In a typical timecode, 7 marker bits are sent,
-- 11 unused bits sent as binary zeros and 42
-- use data bits.  If we assume uniform distribution
-- on the data bits, we get the following prior
-- distribution.
prior :: Signal -> Double
prior Marker      =  7.0 / 60.0
prior (Bit False) = 32.0 / 60.0
prior (Bit True)  = 21.0 / 60.0

signalDist :: Signal -> [Double]
signalDist Marker      = markerDist
signalDist (Bit True)  = oneDist
signalDist (Bit False) = zeroDist

markerDist :: [Double]
markerDist =
    [ 0.7, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9
    , 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9
    , 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.8
    , 0.7, 0.5, 0.5, 0.3, 0.1, 0.1, 0.1, 0.1, 0.3, 0.5
    ]

oneDist :: [Double]
oneDist =
    [ 0.7, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9
    , 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.7, 0.5
    , 0.5, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1
    , 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.5
    ]

zeroDist :: [Double]
zeroDist =
    [ 0.7, 0.9, 0.9, 0.9, 0.9, 0.9, 0.7, 0.5, 0.5, 0.3
    , 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1
    , 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1
    , 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.5
    ]

logLikelyhood :: Signal -> [Bool] -> Double
logLikelyhood s ds = foldr (+) priorP sigPs
  where priorP = log (prior s)
        sigPs  = zipWith f ds (signalDist s)
        f d p  = log (if d then p else (1-p))

estimateSignal :: [Bool] -> Signal
estimateSignal ds = fst $ maximumBy cmp xs
  where cmp x y = compare (snd x) (snd y)
        xs = [ (s, logLikelyhood s ds)
             | s <- sigs
             ]
        sigs = [Marker, Bit False, Bit True]

estimateSignals :: ByteString -> [Signal]
estimateSignals =
  map estimateSignal . splitSignals . Base64.decodeLenient

splitSignals :: ByteString -> [[Bool]]
splitSignals bs = splitInto 40 bs'
  where bs' = BS.foldl f id bs []
        f ss b = ss . g b
        g b x = [ testBit b i
                | i <- reverse [0..7]
                ] ++ x

tc :: Timecode
tc = Timecode
       31
       10
       111
       POS
       0
       15
       False
       False
       DSTNotInEffect

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ showSamples ss
  putStrLn ""
  putStrLn $ unlines $
       map (map (\x -> if x then 'x' else '_')) $
       splitSignals $ Base64.decodeLenient ss
  putStrLn ""
  putStrLn $ show $ estimateSignals ss
  putStrLn $ show $ signalsToTimecode $ estimateSignals ss

  --putStrLn ""
  --putStrLn $ show $ timecodeToSignals tc
  --putStrLn ""
  --putStrLn $ showRawSamples $ timecodeToSamples tc
