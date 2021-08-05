{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Array (Array, (!), listArray)
import qualified Draw as D
import qualified Harmonics as H
import qualified Solver as S

only :: (H.Harmonic -> Bool) -> [[H.FingeredHarmonic]] -> [[H.FingeredHarmonic]]
only pred db =
  let f l = filter (\(_, harm) -> pred harm) l
   in map f db

main :: IO ()
main =
  let db =
        only
          H.isMultiphonic
          [ H.positionsUpTo H.vcDb4 13.0 (H.instPossibilities H.vcDb4)
          , H.positionsUpTo H.cbDb2 13.0 (H.instPossibilities H.cbDb2)
          ]
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
          15
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
            --     S.inWindow 3 (any (H.isHalfHarm . snd)) vcs &&
--                 S.inWindow 3 (any (H.isHalfHarm . snd)) dbs &&
                 H.psetPredPair
                   (\p1 p2 -> (H.scoreHarms p1 p2) >= 1.5)
                   fingeringPairs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= 1.5) vcs &&
                 H.psetPred (\p1 p2 -> (H.scoreHarms p1 p2) >= 1.5) dbs)
   in do let solutions = S.collectSolutions 1 solver
         let firstSol = solutions !! 0
         let vc = map fst firstSol
         let db = map snd firstSol
         let stavesVc = D.harmonicsToStaves D.FClef D.GClef vc
         let stavesDb = D.harmonicsToStaves D.F8Clef D.G8Clef db
         putStrLn $ show (solutions !! 0)
         D.writeStaves "/home/luc/tmp/harmo1.svg" (stavesVc <> stavesDb)
