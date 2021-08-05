{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Draw where

import qualified Data.Fixed           as Fixed
import qualified Data.List            as List
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import qualified Diagrams.Size        as Size
import qualified Harmonics            as Harmonics

data NoteHead
  = Norm
  | Harm
  | Half
  deriving (Show, Eq)

data Clef
  = GClef
  | G8Clef
  | FClef
  | F8Clef
  deriving (Show, Eq)

clef :: Clef -> Double -> Double -> Double -> Diagram B
clef GClef size y x =
  text "y" # fontSizeL size # font "Clefs" # translateY y # translateX x
clef G8Clef size y x =
  text "Y" # fontSizeL size # font "Clefs" # translateY y # translateX x
clef FClef size y x =
  text "s" # fontSizeL size # font "Clefs" # translateY y # translateX x
clef F8Clef size y x =
  text "S" # fontSizeL size # font "Clefs" # translateY y # translateX x

clefPitchMap :: [(Double, (Double, String))]
clefPitchMap =
  [ (0, (0, "n"))
  , (0.5, (0, "+"))
  , (1, (0, "s"))
  , (1.5, (0.5, "d"))
  , (2, (0.5, "n"))
  , (2.5, (0.5, "+"))
  , (3, (1, "b"))
  , (3.5, (1, "d"))
  , (4, (1, "n"))
  , (4.5, (1, "+"))
  , (5, (1.5, "n"))
  , (5.5, (1.5, "+"))
  , (6, (1.5, "s"))
  , (6.5, (2, "d"))
  , (7, (2, "n"))
  , (7.5, (2, "+"))
  , (8, (2.5, "b"))
  , (8.5, (2.5, "d"))
  , (9, (2.5, "n"))
  , (9.5, (2.5, "+"))
  , (10, (3, "b"))
  , (10.5, (3, "d"))
  , (11, (3, "n"))
  , (11.5, (3, "+"))
  ]

classOct :: Double -> (Double, Int)
classOct pitch = (Fixed.mod' pitch 12.0, truncate (pitch / 12.0))

ledgerLines :: Double -> Double -> Diagram B
ledgerLines x y =
  let truncy = fromIntegral (truncate y)
   in if (y >= 0.5) && (y < 6)
        then mempty
        else if (y < 0.5)
               then (p2 ((x - 1.05), truncy)) ~~ (p2 ((x + 1.05), truncy)) <>
                    (ledgerLines x (y + 1))
               else (p2 ((x - 1.05), truncy)) ~~ (p2 ((x + 1.05), truncy)) <>
                    ledgerLines x (y - 1)

roundToQuarter :: Double -> Double
roundToQuarter pitch = (fromIntegral (round (pitch * 2))) / 2

stringNum :: Maybe Int -> Diagram B
stringNum (Just 1) = text "I" # fontSizeL 2 # font "Courier New"
stringNum (Just 2) = text "II" # fontSizeL 2 # font "Courier New"
stringNum (Just 3) = text "III" # fontSizeL 2 # font "Courier New"
stringNum (Just 4) = text "IV" # fontSizeL 2 # font "Courier New"
stringNum _        = mempty

note :: Clef -> Double -> Double -> NoteHead -> Maybe (Int, Double) -> Diagram B
note clef pitch x head strAndOffset =
  let str = fst <$> strAndOffset
      scordaturaOffset =
        (case strAndOffset of
           Just (_, offset) -> offset
           _                -> 0.0)
   in let (pclass, oct) = classOct (roundToQuarter (pitch + scordaturaOffset))
       in case lookup pclass clefPitchMap of
            Nothing -> mempty
            Just (line, acc) ->
              let y = line + (clefOffset clef oct)
                  ledgerY = (fromIntegral (truncate y))
               in notehead head y x <> accidental acc (y + 0.5) (x - 1.4) <>
                  ((ledgerLines x y) # lwL 0.2) <>
                  ((stringNum str) # translateX x # translateY 10)
              where clefOffset GClef oct = 3.5 * (fromIntegral (oct - 5))
                    clefOffset G8Clef oct = 3.5 * (fromIntegral (oct - 4))
                    clefOffset FClef oct =
                      (3.5 * (fromIntegral (oct - 4))) + 2.5
                    clefOffset F8Clef oct =
                      (3.5 * (fromIntegral (oct - 3))) + 2.5

notehead :: NoteHead -> Double -> Double -> Diagram B
notehead Norm y x =
  circle 0.63 # fc black # lwL 0.0 # translateY y # translateX x
notehead Harm y x =
  square 0.65 # rotateBy (1 / 8) # lwL 0.2 # translateY y # translateX x
notehead Half y x =
  square 0.65 # rotateBy (1 / 8) # lwL 0.2 # fc black # translateY y #
  translateX x

--  rotateBy (1 / 8) #
accidental txt y x =
  text txt # fontSizeL 4 # font "Accidentals" # translateY y # translateX x

lineLength n = fromOffsets [unitX * n]

staffLines :: V2 Double -> Diagram B
staffLines n =
  mconcat $
  zipWith
    (\d t -> d # lwL 0.2 # translateY t)
    (take 5 (repeat (lineLength n)))
    [1 ..]

createStaff :: Clef -> V2 Double -> Diagram B
createStaff clefType x =
  staffLines x <>
  case clefType of
    GClef  -> clef clefType size 4.5 1.5
    G8Clef -> clef clefType size 4.5 1.5
    FClef  -> clef clefType size 6.25 1.5
    F8Clef -> clef clefType size 6.25 1.5
  where
    size = 4.5

clefOffset :: Clef -> Double -> Double
clefOffset GClef pitch  = pitch
clefOffset G8Clef pitch = (pitch + 12)
clefOffset FClef pitch  = (pitch + 21)
clefOffset F8Clef pitch = (pitch + 21 + 12)

writeStaves path staves = writeSVG path (vsep 26 staves)

writeSVG path dia =
  renderSVG path Size.absolute (dia # centerXY # pad 1.1 # padY 2)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

extractLoUp ::
     Harmonics.FingeredHarmonic
  -> (Int, [(NoteHead, Double)], [(NoteHead, Double)])
extractLoUp (str, harm) =
  case Harmonics.harmType harm of
    Harmonics.Natural ->
      ( str
      , [(Harm, Harmonics.harmFingering harm)]
      , zip (repeat Norm) (Harmonics.soundingPitches harm))
    Harmonics.HalfHarmonic ->
      let fingered = Harmonics.harmFingering harm
       in ( str
          , [(Half, fingered)]
          , zip
              (repeat Norm)
              (filter (/= fingered) (Harmonics.soundingPitches harm)))
    _ ->
      ( str
      , [(Norm, Harmonics.harmFund harm), (Harm, Harmonics.harmFingering harm)]
      , zip (repeat Norm) (Harmonics.soundingPitches harm))

type ScordaturaOptions = [(Int, Double)]

-- | lookup string for list with string idx and scordatura offset
lookupString :: ScordaturaOptions -> Int -> Maybe (Int, Double)
lookupString assocStrings string =
  (\scor -> (string, scor)) <$> List.lookup string assocStrings

harmonicsToStaves ::
     Clef
  -> Clef
  -> ScordaturaOptions
  -> Maybe [Int]
  -> [Harmonics.FingeredHarmonic]
  -> Diagram B
harmonicsToStaves lowerClef upperClef scorda times harms =
  (vsep
     2
     [upperStaff <> (mconcat upperNotes), lowerStaff <> (mconcat lowerNotes)])
  where
    splitHarms = map extractLoUp harms
    strings = (map fst3 splitHarms)
    lower = (map snd3 splitHarms)
    upper = (map thrd3 splitHarms)
    xfac = 4
    xs =
      case times of
        Nothing  -> [1 * xfac,2 * xfac ..]
        Just lst -> map (\t -> ((t * xfac) + 10)) lst
    xlength = (List.last xs) + 10
    lowerNotes =
      zipWith3
        (\notes x str ->
           mconcat $
           map
             (\(head, pitch) ->
                note
                  lowerClef
                  pitch
                  (fromIntegral x)
                  head
                  (lookupString scorda str))
             notes)
        lower
        xs
        strings
    upperNotes =
      zipWith
        (\notes x ->
           mconcat $
           map
             (\(head, pitch) ->
                note upperClef pitch (fromIntegral x) head Nothing)
             notes)
        upper
        xs
    lowerStaff = createStaff lowerClef (fromIntegral xlength)
    upperStaff = createStaff upperClef (fromIntegral xlength)
-- TODO:
-- not drawing ledger lines multiple times
-- not drawing string number multiple times
-- M and harm numbers
