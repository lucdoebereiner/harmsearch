module Solver
  ( SearchState
  , solve
  , solution
  , create
  , successive
  , allDifferent
  , collectSolutions
  , tupleIdx
  , inWindow
  ) where

data SearchState a b = SearchState
  { solutionFound :: [a]
  , domain :: [b]
  , domainMaxIdx :: Int
  , indexFun :: Int -> [b] -> a
  , predicate :: [a] -> Bool
  , resultIndices :: [Int]
  , solutionsIndices :: [Int]
  , lastSolution :: Bool
  , n :: Int
  }

-- foldable for data?
create ::
     [b] -> Int -> (Int -> [b] -> a) -> Int -> ([a] -> Bool) -> SearchState a b
create domain maxIdx indexFn n predicate =
  SearchState
    { solutionFound = []
    , domain = domain
    , domainMaxIdx = maxIdx
    , indexFun = indexFn
    , predicate = predicate
    , resultIndices = []
    , solutionsIndices = []
    , lastSolution = False
    , n = n
    }

incHead :: [Int] -> [Int]
incHead [] = []
incHead (h:t) = (h + 1) : t

tupleIdx :: (Int, Int) -> Int -> (Int, Int)
tupleIdx (lx, ly) idx =
  let idxY = rem idx ly
   in (quot idx ly, idxY)

mapIndices :: SearchState a b -> [a]
mapIndices state =
  map (\idx -> getIdx idx (domain state)) (reverse (solutionsIndices state))
  where
    getIdx = (indexFun state)

solution :: (Bool, SearchState a b) -> Maybe [a]
solution (False, _) = Nothing
solution (True, s) = Just (solutionFound s)

backtrack :: SearchState a b -> (Bool, SearchState a b)
backtrack state =
  if null indices
    then (False, state)
    else if head indices >= domainMaxIdx state
           then backtrack state {solutionsIndices = tail indices}
           else (True, state {solutionsIndices = incHead indices})
  where
    indices = solutionsIndices state

solveAux :: SearchState a b -> (Bool, SearchState a b)
solveAux state =
  let mappedSolution = mapIndices state
   in if predicate state $ mappedSolution
        then if (length $ solutionsIndices state) >= (n state)
               then ( True
                    , state
                        { solutionFound = mappedSolution
                        , resultIndices = solutionsIndices state
                        })
               else solveAux
                      state {solutionsIndices = 0 : (solutionsIndices state)}
        else case backtrack state of
               (True, btState) -> solveAux btState
               fail@(False, _) -> fail

solve :: SearchState a b -> (Bool, SearchState a b)
solve state =
  if lastSolution state
    then (True, state)
    else let stateInit =
               if null $ solutionsIndices state
                 then state {solutionsIndices = [0]}
                 else state
          in case solveAux stateInit of
               fail@(False, _) -> fail
               (True, solved) ->
                 case backtrack solved of
                   (False, s) -> (True, s {lastSolution = True})
                   (True, s) -> (True, s)

collectSolutions :: Int -> SearchState a b -> [[a]]
collectSolutions 0 _ = []
collectSolutions n state =
  let (found, nextState) = solve state
   in if found
        then (solutionFound nextState) : (collectSolutions (n - 1) nextState)
        else []

--- PREDICATES
allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs

successive :: (Eq a) => ((a, a) -> Bool) -> [a] -> Bool
successive _ [] = True
successive fun lst = all fun ((zip <*> tail) lst)

windowLst :: Int -> [a] -> [[a]]
windowLst i lst
  | i > length lst = []
  | otherwise = (take i lst) : (windowLst i (tail lst))

inWindow :: Int -> ([a] -> Bool) -> [a] -> Bool
inWindow winSize pred lst = all pred $ windowLst winSize lst
