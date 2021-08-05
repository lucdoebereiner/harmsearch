{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array            (Array, listArray, (!))
import qualified Draw                  as D
import qualified Harmonics             as H
import qualified Solver                as S
import           System.Environment
import           System.Random
import           System.Random.Shuffle

only :: (H.Harmonic -> Bool) -> [H.FingeredHarmonic] -> [H.FingeredHarmonic]
only pred db = filter (\(_, harm) -> pred harm) db

withStrings :: [Int] -> [H.FingeredHarmonic] -> [H.FingeredHarmonic]
withStrings strings db = filter (\(str, _) -> elem str strings) db

shuffled :: Int -> [a] -> [a]
shuffled seed lst = shuffle' lst (length lst) (mkStdGen seed)

line :: Num a => a -> a -> a -> a
line x from to = ((to - from) * x) + from

-- | start and end extra percentage
lineStartEnd :: Double -> Double -> Double -> Double -> Double -> Double
lineStartEnd x from to startPerc endPerc =
  if x < startPerc
    then from
    else if (x + endPerc >= 1.0)
           then to
           else let scaledX = (x - startPerc) / (1 - (startPerc + endPerc))
                 in ((to - from) * scaledX) + from

main :: IO ()
main =
  let db =
        [ shuffled 386 $
          only (\h -> H.isMultiphonic h && not (H.isArtificial h)) $
          withStrings [2, 3, 4] $
          H.positionsUpTo H.vcDb4 13.0 (H.instPossibilities H.vcDb4)
        , shuffled 294 $
          only (\h -> H.isMultiphonic h && not (H.isArtificial h)) $
          H.positionsUpTo H.cbDb2 13.0 (H.instPossibilities H.cbDb2)
        ]
      thresh = 2
      n = 8
      lengthVc = length (db !! 0)
      lengthCb = length (db !! 1)
      vcArray = listArray (0, lengthVc - 1) (db !! 0)
      cbArray = listArray (0, lengthCb - 1) (db !! 1)
      maxIdx = (lengthVc * lengthCb) - 1
      indexTup = S.tupleIdx (lengthVc, lengthCb)
      -- indexFun i dom =
      --   let (ix, iy) = indexTup i
      --    in ((dom !! 0) !! ix, (dom !! 1) !! iy)
      indexFun i dom =
        let (ix, iy) = indexTup i
         in (vcArray ! ix, cbArray ! iy)
      solver =
        S.create
          [vcArray, cbArray]
          maxIdx
          indexFun
          n
          (\fingeringPairs ->
             let vcs = map fst fingeringPairs
                 dbs = map snd fingeringPairs
              in S.allDifferent vcs &&
                 S.allDifferent dbs &&
                 S.successive (H.transitionCheck H.vcDb4) vcs &&
                 S.successive (H.transitionCheck H.cbDb2) dbs &&
                 -- S.inWindow 3 (any (H.isMultiphonic . snd)) vcs &&
                 -- S.inWindow 3 (any (H.isMultiphonic . snd)) dbs &&
                 -- S.inWindow 3 (any (H.isNatMultiphonic . snd)) vcs &&
                 -- S.inWindow 3 (any (H.isNatMultiphonic . snd)) dbs &&
                 -- S.inWindow 2 (any (H.isHalfHarm . snd)) vcs &&
                 -- S.inWindow 2 (any (H.isHalfHarm . snd)) dbs &&
                 H.psetPredPairIdx
                   (\i p1 p2 ->
                      let x = (fromIntegral i) / (fromIntegral n)
                          thisOverlap = lineStartEnd x 0.0 0.15 0.25 0.2
                          thisThresh = lineStartEnd x 5.0 1.5 0.25 0.2
                       in (H.scoreOverlap p1 p2) >= thisOverlap &&
                          (H.scoreHarms p1 p2) >= thisThresh)
                   fingeringPairs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= thresh) vcs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= thresh) dbs)
   in do let solutions = S.collectSolutions 1 solver
         let firstSol = solutions !! 0
         let vc = map fst firstSol
         let db = map snd firstSol
         let stavesVc = D.harmonicsToStaves D.FClef D.GClef vc
         let stavesDb = D.harmonicsToStaves D.F8Clef D.GClef db
         putStrLn $ show (solutions !! 0)
         args <- getArgs
         D.writeStaves (args !! 0) (stavesVc <> stavesDb)
