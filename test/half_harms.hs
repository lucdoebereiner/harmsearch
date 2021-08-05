{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array (Array, (!), listArray)
import qualified Draw as D
import qualified Harmonics as H
import qualified Solver as S
import System.Environment
import System.Random
import System.Random.Shuffle

only :: (H.Harmonic -> Bool) -> [H.FingeredHarmonic] -> [H.FingeredHarmonic]
only pred db = filter (\(_, harm) -> pred harm) db

withStrings :: [Int] -> [H.FingeredHarmonic] -> [H.FingeredHarmonic]
withStrings strings db = filter (\(str, _) -> elem str strings) db

shuffled :: Int -> [a] -> [a]
shuffled seed lst = shuffle' lst (length lst) (mkStdGen seed)

main :: IO ()
main =
  let db =
        [ shuffled 20 $
--          only H.isHalfHarm $
--          withStrings [3, 4] $
          H.positionsUpTo H.vcDb4 13.0 (H.instPossibilities H.vcDb4)
        , shuffled 183 $
--          only H.isHalfHarm $
  --        withStrings [1, 2] $
          H.positionsUpTo H.cbDb2 13.0 (H.instPossibilities H.cbDb2)
        ]
      thresh = 1.75
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
          13
          (\fingeringPairs ->
             let vcs = map fst fingeringPairs
                 dbs = map snd fingeringPairs
              in S.allDifferent vcs &&
                 S.allDifferent dbs &&
                 S.successive (H.transitionCheck H.vcDb4) vcs &&
                 S.successive (H.transitionCheck H.cbDb2) dbs &&
                 -- S.inWindow 3 (any (H.isMultiphonic . snd)) vcs &&
                 -- S.inWindow 3 (any (H.isMultiphonic . snd)) dbs &&
                 -- S.inWindow 4 (any (H.isNatMultiphonic . snd)) vcs &&
                 -- S.inWindow 4 (any (H.isNatMultiphonic . snd)) dbs &&
                 S.inWindow 3 (any (H.isHalfHarm . snd)) vcs &&
                 S.inWindow 3 (any (H.isHalfHarm . snd)) dbs &&
                 H.psetPredPair
                   (\p1 p2 -> (H.scoreHarms p1 p2) >= thresh)
                   fingeringPairs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= thresh) vcs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= thresh) dbs)
   in do let solutions = S.collectSolutions 1 solver
         let firstSol = solutions !! 0
         let vc = map fst firstSol
         let db = map snd firstSol
         let stavesVc = D.harmonicsToStaves D.FClef D.GClef vc
         let stavesDb = D.harmonicsToStaves D.F8Clef D.G8Clef db
         putStrLn $ show (solutions !! 0)
         args <- getArgs
         D.writeStaves (args !! 0) (stavesVc <> stavesDb)
