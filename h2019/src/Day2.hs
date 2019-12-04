module Day2 where

import qualified Data.Map.Strict                    as Map
import           Data.Map.Strict                     ( Map )
import           Data.Function                       ( (&) )

input :: [Int] -> Int -> Int -> Map Int Int
input a v n = Map.fromList (zip [0 ..] a) & Map.insert 2 v & Map.insert 1 n

op :: Int -> Maybe (Int -> Int -> Int)
op 1 = Just (+)
op 2 = Just (*)
op _ = Nothing

run :: Int -> Map Int Int -> Maybe (Map Int Int)
run i m = Map.lookup i m >>= \case
    99 -> pure m
    n  -> do
        aIdx <- Map.lookup (i + 1) m
        aVal <- Map.lookup aIdx m
        bIdx <- Map.lookup (i + 2) m
        bVal <- Map.lookup bIdx m
        rIdx <- Map.lookup (i + 3) m
        f    <- op n
        run (i + 4) (Map.insert rIdx (f aVal bVal) m)

part1 = run 0 (input list 2 12) >>= Map.lookup 0

part2Input :: [(Int, Int)]
part2Input = do
    a <- [0 .. 99]
    b <- [0 .. 99]
    pure (a, b)

part2 = filter (\(_, _, c) -> c == Just 19690720) $ map
    (\(a, b) -> (a, b, run 0 (input list a b) >>= Map.lookup 0))
    part2Input

list :: [Int]
list =
    [ 1
    , 0
    , 0
    , 3
    , 1
    , 1
    , 2
    , 3
    , 1
    , 3
    , 4
    , 3
    , 1
    , 5
    , 0
    , 3
    , 2
    , 10
    , 1
    , 19
    , 1
    , 19
    , 9
    , 23
    , 1
    , 23
    , 13
    , 27
    , 1
    , 10
    , 27
    , 31
    , 2
    , 31
    , 13
    , 35
    , 1
    , 10
    , 35
    , 39
    , 2
    , 9
    , 39
    , 43
    , 2
    , 43
    , 9
    , 47
    , 1
    , 6
    , 47
    , 51
    , 1
    , 10
    , 51
    , 55
    , 2
    , 55
    , 13
    , 59
    , 1
    , 59
    , 10
    , 63
    , 2
    , 63
    , 13
    , 67
    , 2
    , 67
    , 9
    , 71
    , 1
    , 6
    , 71
    , 75
    , 2
    , 75
    , 9
    , 79
    , 1
    , 79
    , 5
    , 83
    , 2
    , 83
    , 13
    , 87
    , 1
    , 9
    , 87
    , 91
    , 1
    , 13
    , 91
    , 95
    , 1
    , 2
    , 95
    , 99
    , 1
    , 99
    , 6
    , 0
    , 99
    , 2
    , 14
    , 0
    , 0
    ]

