module Day2 where

import           Control.Monad.RWS.Strict            ( RWS
                                                     , execRWS
                                                     )
import           Control.Monad.Reader                ( ask )
import           Control.Monad.State.Strict          ( get
                                                     , put
                                                     )
import           Control.Monad.Writer.Strict         ( tell )
import           Data.IntMap.Strict                  ( IntMap
                                                     , (!)
                                                     )
import qualified Data.IntMap.Strict                 as IntMap
import           Data.List.Split                     ( splitOn )
import           Data.Function                       ( (&) )

type Input = Int
type Address = Int
type Value = Int

type IntCode = IntMap Value
type IntCodeM = RWS Input [Int] IntCode

data Command
    = Add Param Param Address
    | Multiply Param Param Address
    | Input Address
    | Output Param
    | JumpTrue Param Param
    | JumpFalse Param Param
    | CmpLT Param Param Address
    | CmpEQ Param Param Address
    | Halt

data Param
    = Address Address
    | Value Value

main :: IO ()
main = do
    f <- readFile "./day5.txt"
    let program =
            f & splitOn "," & map (read @Int) & zip [0 ..] & IntMap.fromList
    print $ part1 program
    print $ part2 program

part1 :: IntCode -> Either [Int] Int
part1 = runProgram 1

part2 :: IntCode -> Either [Int] Int
part2 = runProgram 5

runProgram :: Int -> IntCode -> Either [Int] Int
runProgram input program =
    let (_, outputs) = execRWS (exec 0) input program
    in  if
            | null outputs              -> Left []
            | any (/= 0) (init outputs) -> Left outputs
            | otherwise                 -> Right $ last outputs

exec :: Address -> IntCodeM ()
exec i = do
    program <- get
    case getCommand program of
        Add p1 p2 out -> do
            let result = getVal program p1 + getVal program p2
            put $ IntMap.insert out result program
            exec (i + 4)
        Multiply p1 p2 out -> do
            let result = getVal program p1 * getVal program p2
            put $ IntMap.insert out result program
            exec (i + 4)
        Input out -> do
            input <- ask
            put $ IntMap.insert out input program
            exec (i + 2)
        Output p1 -> do
            tell [getVal program p1]
            exec (i + 2)
        JumpTrue p1 p2 -> if getVal program p1 /= 0
            then exec $ getVal program p2
            else exec (i + 3)
        JumpFalse p1 p2 -> if getVal program p1 == 0
            then exec $ getVal program p2
            else exec (i + 3)
        CmpLT p1 p2 out -> do
            let result = if getVal program p1 < getVal program p2 then 1 else 0
            put $ IntMap.insert out result program
            exec (i + 4)
        CmpEQ p1 p2 out -> do
            let result =
                    if getVal program p1 == getVal program p2 then 1 else 0
            put $ IntMap.insert out result program
            exec (i + 4)
        Halt -> pure ()
  where
    getVal :: IntCode -> Param -> Value
    getVal program = \case
        Address a -> program ! a
        Value   v -> v
    getCommand :: IntCode -> Command
    getCommand program =
        let (modes, opcode) = (program ! i) `divMod` 100
        in
            case opcode of
                1 -> Add (parseParam modes 0) (parseParam modes 1) (rawParam 2)
                2 -> Multiply (parseParam modes 0)
                              (parseParam modes 1)
                              (rawParam 2)
                3 -> Input (rawParam 0)
                4 -> Output (parseParam modes 0)
                5 -> JumpTrue (parseParam modes 0) (parseParam modes 1)
                6 -> JumpFalse (parseParam modes 0) (parseParam modes 1)
                7 -> CmpLT (parseParam modes 0)
                           (parseParam modes 1)
                           (rawParam 2)
                8 -> CmpEQ (parseParam modes 0)
                           (parseParam modes 1)
                           (rawParam 2)
                99 -> Halt
                _  -> error $ "Invalid opcode: " ++ show opcode
      where
        rawParam digit = program ! (i + digit + 1)
        parseParam modes digit =
            getParam (getMode digit modes) (rawParam digit)

getParam :: Int -> Int -> Param
getParam 0    parameter = Address parameter
getParam 1    parameter = Value parameter
getParam mode _         = error $ "Invalid parameter mode: " ++ show mode

getMode :: Int -> Int -> Int
getMode d x = (x `div` 10 ^ d) `mod` 10
