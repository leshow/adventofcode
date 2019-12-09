module Day6 where

import qualified Data.Map.Strict                    as Map
import           Data.Map.Strict                     ( Map
                                                     , (!?)
                                                     , (!)
                                                     )
import           Data.Function                       ( (&) )
import           Data.List.Split                     ( splitOn )
import           Data.Set                            ( Set
                                                     , (\\)
                                                     )
import qualified Data.Set                           as Set

main :: IO ()
main = do
    input <- readFile "./day6.txt"
    let orbits = parseOrbits input
    print $ part1 orbits
    print $ part2 orbits "YOU" "SAN"

type Node = String
type Orbits = Map Node Node
type Visited = Set Node

parseOrbits :: String -> Orbits
parseOrbits = Map.fromList . map parseLine . lines

parseLine :: String -> (Node, Node)
parseLine = (\[a, b] -> (b, a)) . splitOn ")"

part1 :: Orbits -> Int
part1 orbits = Map.keys orbits & map (countOrbit orbits) & sum

part2 :: Orbits -> Node -> Node -> Int
part2 orbits a b = Set.size justA + Set.size justB
  where
    aTrans = transfers orbits (orbits ! a) Set.empty
    bTrans = transfers orbits (orbits ! b) Set.empty
    justA  = aTrans \\ bTrans
    justB  = bTrans \\ aTrans



transfers :: Orbits -> Node -> Visited -> Visited
transfers orbits node visited = case orbits !? node of
    Just newnode -> transfers orbits newnode $ Set.insert node visited
    Nothing      -> visited

countOrbit :: Orbits -> Node -> Int
countOrbit orbits node = case orbits !? node of
    Just newnode -> 1 + countOrbit orbits newnode
    Nothing      -> 0
