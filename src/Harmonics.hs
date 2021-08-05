module Harmonics where

import qualified Data.Fixed         as Fixed
import qualified Data.Map           as Map
import qualified Solver             as Solver
import           System.Environment

type Pitch = Double

type Frequency = Double

type FreqFac = Double

type Interval = Double

quantize :: Double -> Double -> Double
quantize quant x
  | quant == 0.0 = x
  | otherwise = (fromInteger . truncate $ x / quant + 0.5) * quant

midicps :: Pitch -> Frequency
midicps note = 440.0 * (2.0 ** ((note - 69.0) * 5 / 6 / 10))

cpsmidi :: Frequency -> Pitch
cpsmidi freq = (logBase 2.0 (freq * 1 / 44 / 10)) * 12.0 + 69.0

nodes :: Int -> [FreqFac]
nodes harm = [(fromIntegral i) / (fromIntegral harm) | i <- [1 .. (harm - 1)]]

harmPitch :: Pitch -> Int -> Pitch
harmPitch pitch harm = cpsmidi $ (midicps pitch) * (fromIntegral harm)

reduceInterval :: Pitch -> Pitch -> Interval
reduceInterval p1 p2 = Fixed.mod' ((max p1 p2) - (min p1 p2)) 12.0

data ResultingHarmonics
  = Single (Int, Pitch)
  | Multi [(Int, Pitch)]
  deriving (Show, Eq)

data HarmonicType
  = Natural
  | Artificial
  | HalfHarmonic -- only for natural harmonics!
  deriving (Show, Eq)

data Harmonic = Harm
  { harmType      :: HarmonicType
  , harmFund      :: Pitch
  , harmHarms     :: ResultingHarmonics
  , harmFingering :: Pitch
  } deriving (Show, Eq)

withHarmType :: HarmonicType -> Harmonic -> Harmonic
withHarmType hType harm = harm {harmType = hType}

halfHarmInterval :: Harmonic -> Maybe Interval
halfHarmInterval harm =
  case harmHarms harm of
    Multi _       -> Nothing
    Single (_, p) -> Just $ reduceInterval (harmFingering harm) p

soundingPitches :: Harmonic -> [Pitch]
soundingPitches h =
  let fing =
        case harmType h of
          HalfHarmonic -> [(harmFingering h)]
          _            -> []
   in case harmHarms h of
        Single (_, p) -> fing ++ [p]
        Multi lst     -> fing ++ (map snd lst)

touchPoints :: Int -> [FreqFac]
touchPoints harm = filterDuplicates (nodes harm) (collectNodes (harm - 1))
  where
    filterDuplicates lst filLst = filter (\e -> not $ elem e filLst) lst
    collectNodes 0 = []
    collectNodes h = (nodes h) ++ (collectNodes (h - 1))

factorToPitch :: Pitch -> FreqFac -> Pitch
factorToPitch fundamental fac = cpsmidi $ (midicps fundamental) * (1 / fac)

harmTouchPoints :: Pitch -> Int -> [Harmonic]
harmTouchPoints fundamental harm =
  map
    (Harm Natural fundamental (Single (harm, (harmPitch fundamental harm))) .
     factorToPitch fundamental)
    (touchPoints harm)

harmsFromTo :: Pitch -> Int -> Int -> [Harmonic]
harmsFromTo fund startHarm endHarm =
  concatMap (harmTouchPoints fund) [i | i <- [startHarm .. endHarm]]

multiphonicHarms :: [([Int], FreqFac)]
multiphonicHarms =
  [ ([7, 13, 6], 0.846)
  , ([6, 11, 5], 0.818)
  , ([5, 9, 13, 4], 0.769)
  , ([4, 11, 7, 3], 0.727)
  , ([7, 10, 13, 3], 0.692)
  , ([3, 11, 8], 0.636)
  , ([3, 8, 13, 5], 0.615)
  , ([5, 12, 7], 0.583)
  ]

multisForFund :: Pitch -> [Harmonic]
multisForFund fundamental =
  map
    (\(harms, pos) ->
       let fingering = cpsmidi $ fundFreq * (1 / pos)
        in Harm
             Natural
             fundamental
             (Multi (zip harms (map (harmPitch fundamental) harms)))
             fingering)
    multiphonicHarms
  where
    fundFreq = midicps fundamental

data Instrument = Instrument
  { strings   :: [(Int, Pitch)]
  , rangeOct1 :: Interval
  }

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n
  | factors == [] = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where
    factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n - 1]

countFactors :: [Int] -> [(Int, Int)]
countFactors [] = []
countFactors lst = countFactors' lst Map.empty
  where
    countFactors' :: [Int] -> Map.Map Int Int -> [(Int, Int)]
    countFactors' [] acc = Map.toList acc
    countFactors' (f:rest) acc =
      case Map.lookup f acc of
        Nothing -> countFactors' rest (Map.insert f 1 acc)
        Just n  -> countFactors' rest (Map.adjust (+ 1) f acc)

-- euler gradus
gradus :: Int -> Int
gradus n = foldl (\sum (p, a) -> sum + a * (p - 1)) 0 factors
  where
    factors = countFactors $ primeFactors n

type Ratio = (Int, Int)

gradusRatio :: Ratio -> Int
gradusRatio (nom, denom) = gradus $ lcm nom denom

intervalRatios :: [Ratio]
intervalRatios =
  [ (1, 1) -- unison
  , (16, 15) -- min 2nd
  , (10, 9) -- maj 2nd
  , (6, 5) -- min 3rd
  , (5, 4) -- maj 3rd
  , (4, 3) -- fourth
  , (45, 32) -- tritone
  , (3, 2) -- fifth
  , (8, 5) -- minor 6th
  , (5, 3) -- major 6th
  , (16, 9) -- min 7th
  , (15, 8) -- maj 7th
  , (2, 1) -- octave
  ]

findClosest :: (Ord a, Num a) => a -> [a] -> a
findClosest num lst = foldr1 min $ map (\x -> abs (x - num)) lst

closestRatio :: Pitch -> Pitch -> Ratio
closestRatio p1 p2 =
  snd $
  findClosest
    ratio
    (map
       (\(x, y) -> ((fromIntegral x) / (fromIntegral y), (x, y)))
       intervalRatios)
  where
    reduced = reduceInterval p1 p2
    ratio = midicps reduced / midicps 0
    findClosest num lst =
      foldr1
        (\(x1, r1) (x2, r2) ->
           if (x1 < x2)
             then (x1, r1)
             else (x2, r2))
        (map (\(x, r) -> (abs (x - num), r)) lst)

gradusQuantified :: Pitch -> Pitch -> Int
gradusQuantified p1 p2 = gradusRatio $ closestRatio p1 p2

gradusCombinations :: [Pitch] -> [Pitch] -> [Int]
gradusCombinations ps1 ps2 = gradusQuantified <$> ps1 <*> ps2

gradusSet :: [Pitch] -> [Int]
gradusSet ps = gradusQuantified <$> ps <*> ps

intervalContent :: [Pitch] -> [Pitch] -> [Interval]
intervalContent ps1 ps2 = reduceInterval <$> ps1 <*> ps2

nOct :: [Pitch] -> [Pitch] -> Int
nOct ps1 ps2 = length $ filter (== 0.0) $ intervalContent ps1 ps2

nInharm :: [Pitch] -> [Pitch] -> Int
nInharm ps1 ps2 = length $ filter (>= 9) $ gradusCombinations ps1 ps2

inharmOrOct :: [Pitch] -> [Pitch] -> Bool
inharmOrOct ps1 ps2 =
  all (\g -> (g >= 9) || (g == 0)) $ gradusCombinations ps1 ps2

count :: (a -> Bool) -> [a] -> Int
count pred lst =
  sum $
  map
    (\e ->
       if pred e
         then 1
         else 0)
    lst

scoreHarms :: [Pitch] -> [Pitch] -> Double
scoreHarms ps1 ps2 =
  let grades = gradusCombinations ps1 ps2
   in product
        [ (score (any (== 0)) 1.7 grades)
        , 2 ** (fromIntegral (count (>= 9) grades))
        , 0.5 ** (fromIntegral (count (\g -> g > 0 && g < 9) grades))
        ]
  where
    score :: ([Int] -> Bool) -> Double -> [Int] -> Double
    score pred sc lst =
      if pred lst
        then sc
        else 1.0

scoreOverlap :: [Pitch] -> [Pitch] -> Double
scoreOverlap ps1 ps2 =
  let grades = gradusCombinations ps1 ps2
   in (fromIntegral (count (== 0) grades)) / (fromIntegral (length grades))

psetPred :: ([Pitch] -> [Pitch] -> Bool) -> [FingeredHarmonic] -> Bool
psetPred func harms =
  Solver.successive
    (\((_, h1), (_, h2)) -> func (soundingPitches h1) (soundingPitches h2))
    harms

psetPredIdx :: (Int -> [Pitch] -> [Pitch] -> Bool) -> [FingeredHarmonic] -> Bool
psetPredIdx func harms =
  Solver.successive
    (\((i1, (_, h1)), (_, (_, h2))) ->
       func i1 (soundingPitches h1) (soundingPitches h2)) $
  (zip [1 ..] harms)

psetPredPair ::
     ([Pitch] -> [Pitch] -> Bool)
  -> [(FingeredHarmonic, FingeredHarmonic)]
  -> Bool
psetPredPair func harms =
  all
    (\((_, h1), (_, h2)) -> func (soundingPitches h1) (soundingPitches h2))
    harms

psetPredPairIdx ::
     (Int -> [Pitch] -> [Pitch] -> Bool)
  -> [(FingeredHarmonic, FingeredHarmonic)]
  -> Bool
psetPredPairIdx func harms =
  all
    (\(i, ((_, h1), (_, h2))) ->
       func i (soundingPitches h1) (soundingPitches h2)) $
  zip [1 ..] harms

artHarms :: Interval -> Pitch -> [Harmonic]
artHarms range pitch =
  map (withHarmType Artificial) $
  filter
    (\h ->
       let dist = ((harmFingering h) - pitch)
        in (dist <= range) && (dist >= 2))
    ((harmsFromTo pitch 3 7) ++ (multisForFund pitch))

type FingeredHarmonic = (Int, Harmonic)

-- first octave only
collectArtHarms :: Instrument -> [FingeredHarmonic]
collectArtHarms inst =
  concatMap
    (\(str, fund) -> collectArtHarmsString' [] str (fund + 0.5) (fund + 12))
    (strings inst)
  where
    collectArtHarmsString' ::
         [FingeredHarmonic] -> Int -> Pitch -> Pitch -> [FingeredHarmonic]
    collectArtHarmsString' acc string start end =
      if (start > end)
        then acc
        else let harms = zip (repeat string) $ artHarms (rangeOct1 inst) start
              in collectArtHarmsString' (harms ++ acc) string (start + 0.5) end

instPossibilities :: Instrument -> [FingeredHarmonic]
instPossibilities instrument = nats ++ arts ++ multis ++ halfHarms
  where
    arts = collectArtHarms instrument
    nats =
      concatMap
        -- (\(str, fund) -> zip (repeat str) (harmsFromTo fund 3 13))
        (\(str, fund) -> zip (repeat str) (harmsFromTo fund 3 11))
        (strings instrument)
    multis =
      concatMap
        (\(str, fund) -> zip (repeat str) (multisForFund fund))
        (strings instrument)
        -- filter those hals harms that are oct, fifth or forth
    halfHarms =
      filter
        (\hH ->
           let ps = soundingPitches . snd $ hH
            in (gradusQuantified (ps !! 0) (ps !! 1)) > 5)
        (map (\(str, h) -> (str, withHarmType HalfHarmonic h)) nats)

-- | possible adjacent string fingerings
adjacentStrings :: (Int, Int) -> Bool
adjacentStrings str =
  case str of
    (1, 2) -> True
    (2, 1) -> True
    (2, 3) -> True
    (3, 2) -> True
    (3, 4) -> True
    (4, 3) -> True
    _      -> False

-- instrument definitions
vcDb4 :: Instrument
vcDb4 = Instrument [(4, 37), (3, 43), (2, 50), (1, 57)] 5

cbDb2 :: Instrument
cbDb2 = Instrument [(4, 28), (3, 33), (2, 37), (1, 43)] 4

position :: Instrument -> Int -> Pitch -> Maybe Double
position inst str pitch =
  case lookup str (strings inst) of
    Just fund ->
      if pitch >= fund
        then Just (pitch - fund)
        else Nothing
    _ -> Nothing

harmonicPosition :: Instrument -> FingeredHarmonic -> Maybe [Double]
harmonicPosition inst (str, harm) =
  case harmType harm of
    Natural -> fmap (replicate 1) $ position inst str (harmFingering harm)
    HalfHarmonic -> fmap (replicate 1) $ position inst str (harmFingering harm)
    _ ->
      sequence $
      [ position inst str (harmFund harm)
      , position inst str (harmFingering harm)
      ]

testMaybe :: (a -> Bool) -> Maybe a -> Bool
testMaybe _ Nothing  = False
testMaybe f (Just v) = f v

positionsUpTo ::
     Instrument -> Double -> [FingeredHarmonic] -> [FingeredHarmonic]
positionsUpTo inst limit harmLst =
  filter
    (\harm -> testMaybe (all (\p -> p <= limit)) $ harmonicPosition inst harm)
    harmLst

checkArtHarmTransition :: Pitch -> (Pitch, Pitch) -> Double -> Bool
checkArtHarmTransition p1 (art1, art2) range =
  let artMin = min art1 art2
      artMax = max art1 art2
      artRange = artMax - artMin
  -- narrow mulitphonic / art harm, finger also higher/lower
   in if artRange <= (range / 2)
        then if p1 <= artMin
               then (artMin - p1) <= (range / 2)
               else if p1 >= artMax
                      then (p1 - artMax) <= (range / 2)
                      else True
  -- not narrow, finger between
        else if (p1 >= artMin) && (p1 <= artMax)
               then True
               else False

possibleTransition :: Instrument -> FingeredHarmonic -> FingeredHarmonic -> Bool
possibleTransition inst (str1, harm1) (str2, harm2) =
  if adjacentStrings (str1, str2)
    then let pos1 = harmonicPosition inst (str1, harm1)
             pos2 = harmonicPosition inst (str2, harm2)
          in case (pos1, pos2) of
               (Just [p1], Just [p2]) -> (abs (p1 - p2)) <= (rangeOct1 inst)
               (Just [p1], Just [art1, art2]) ->
                 checkArtHarmTransition p1 (art1, art2) (rangeOct1 inst)
               (Just [art1, art2], Just [p1]) ->
                 checkArtHarmTransition p1 (art1, art2) (rangeOct1 inst)
               _ -> False
    else False

transitionCheck :: Instrument -> (FingeredHarmonic, FingeredHarmonic) -> Bool
transitionCheck inst (f1, f2) = possibleTransition inst f1 f2

-- TODO other possibilities
-- which transitions are possible?
-- define meaningful constraints
-- search and notation for one instrument (DONE)
-- seach and notation for two instruments
-- half harm, mini gliss, artifical harm, artificial multi
-- function that gives closest harm relation of two pitches
-- constraints
isMultiphonic :: Harmonic -> Bool
isMultiphonic harm =
  case harmType harm of
    HalfHarmonic -> True
    _ ->
      case harmHarms harm of
        Multi _ -> True
        _       -> False

isArtificial :: Harmonic -> Bool
isArtificial harm =
  case harmType harm of
    Artificial -> True
    _          -> False

isNatural :: Harmonic -> Bool
isNatural harm =
  case harmType harm of
    Natural -> True
    _       -> False

isNatMultiphonic :: Harmonic -> Bool
isNatMultiphonic harm =
  case (harmHarms harm, harmType harm) of
    (Multi _, Natural) -> True
    _                  -> False

isHalfHarm :: Harmonic -> Bool
isHalfHarm harm =
  case harmType harm of
    HalfHarmonic -> True
    _            -> False
