module Advent2022.Day11 where

import Text.ParserCombinators.ReadP
import Advent2022.Day (Day, makeDay)
import Advent2022.Misc (zipWithDefault)
import Advent2022.ParseUtils (parseInt, runReadP)
import Data.Functor (($>))
import Data.Function ((&))
import Data.List (partition, foldl', sort)

type Throw = Int -> Int
type Operation = Int -> Int
type MonkeyProgram = (Operation, Throw, Int)
type Loot = [Int]
type Monkey = (Loot, MonkeyProgram)

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [Monkey] -> Int
solve1 monkeys = product topTwo where
    (loots, programs) = unzip monkeys
    programs' = map (\(o, b, c) -> (accountForRelief o, b, c)) programs
    roundsScores = fst $ processRounds 20 programs' loots
    topTwo = take 2 $ reverse $ sort roundsScores

solve2 :: [Monkey] -> Int
solve2 monkeys = product topTwo where
    (loots, programs) = unzip monkeys
    roundsScores = fst $ processRounds 10000 programs loots
    topTwo = take 2 $ reverse $ sort roundsScores

processRounds :: Int -> [MonkeyProgram] -> [Loot] -> ([Int], [Loot])
processRounds n programs loot = foldl' (uncurry processRounds') ([], loot) $ replicate n programs

processRounds' :: [Int] -> [Loot] -> [MonkeyProgram] -> ([Int], [Loot])
processRounds' nInspects l program = (nInspects'', l') where
    (nInspects', l') = processRound program l
    nInspects'' = zipWithDefault (+) 0 nInspects' nInspects

-- Not a big fan: the two parameters must have the same length
processRound :: [MonkeyProgram] -> [Loot] -> ([Int], [Loot])
processRound programs = processRound' (zip [0..] programs) [] []

-- program and monkey number, heap, loop, resulting loot
processRound' :: [(Int, MonkeyProgram)] -> [(Int, Int)] -> [Int] -> [Loot] -> ([Int], [Loot])
processRound' ((n, prog):xs) heap nInspecs (l:ls) = processRound' xs heap' (length thrown:nInspecs) ls' where
    thrown = throwItems prog l
    (thrownToNextMonkeys, thrownToPreviousMonkeys) = partition ((> n) . snd) thrown
    heap' = heap ++ thrownToPreviousMonkeys
    ls' = mergeHeapToLootFrom (n+1) ls thrownToNextMonkeys
processRound' _ heap nInspecs [] = (reverse nInspecs, resolveHeap heap)
processRound' [] _ _ _ = undefined

-- [(item, dest)]
mergeHeapToLootFrom :: Int -> [Loot] -> [(Int, Int)] -> [Loot]
mergeHeapToLootFrom _ l [] = l
mergeHeapToLootFrom n [] heap = loot'' : mergeHeapToLootFrom (n+1) [] heap' where
    (loot', heap') = partition ((==n) . snd) heap
    loot'' = map fst loot'
mergeHeapToLootFrom n (loot:loots) heap = (loot ++ loot'') : mergeHeapToLootFrom (n+1) loots heap' where
    (loot', heap') = partition ((==n) . snd) heap
    loot'' = map fst loot'

-- [(item, dest)]
resolveHeap :: [(Int, Int)] -> [Loot]
resolveHeap = resolveHeapFrom 0

resolveHeapFrom :: Int -> [(Int, Int)] -> [Loot]
resolveHeapFrom n heap = reverse $ resolveHeap' [] heap n

resolveHeap' :: [Loot] -> [(Int, Int)] -> Int -> [Loot]
resolveHeap' acc [] _ = acc
resolveHeap' acc heap n = resolveHeap' (loot' : acc) heap' (n+1) where
    (loot, heap') = partition ((==n) . snd) heap
    loot' = map fst loot

throwItems :: MonkeyProgram -> [Int] -> [(Int, Int)]
throwItems = map . throwItem

throwItem :: MonkeyProgram -> Int -> (Int, Int)
throwItem (operation, throw, d) level = (level', destination) where
    level' = operation level `mod` d
    destination = throw level'

accountForRelief :: Operation -> Operation
accountForRelief f x = f x `div` 3

-- Parse

parse :: String -> [Monkey]
parse = runReadP (parseMonkeys <* skipSpaces <* eof)

parseMonkeys :: ReadP [Monkey]
parseMonkeys = fixMonkeysDivisor <$> sepBy parseMonkey (string "\n\n")

fixMonkeysDivisor :: [Monkey] -> [Monkey]
fixMonkeysDivisor monkeys = zip loots programs' where
    (loots, programs) = unzip monkeys
    monkeysDivisors = foldr (lcm . thd3) 1 programs
    programs' = map (setThd3 monkeysDivisors) programs
    thd3 (_, _, c) = c
    setThd3 c (a, b, _) = (a, b, c)

parseMonkey :: ReadP Monkey
parseMonkey = do
    _ <- string "Monkey " *> parseInt *> char ':' *> skipSpaces
    startingItems <- parseItems
    _ <- skipSpaces
    operation <- parseOperation
    _ <- skipSpaces
    (throwTest, divisor) <- parseThrowTest
    return (startingItems, (operation, throwTest, divisor))

parseItems :: ReadP [Int]
parseItems = skipSpaces *> string "Starting items: " *> sepBy parseInt (string ", ")

parseOperation :: ReadP Operation
parseOperation = (&) <$> (skipSpaces *> string "Operation: new = old " *> parseOperator) <*> (skipSpaces *> parseOperand)

parseOperator :: ReadP (Int -> Int -> Int)
parseOperator = choice [char '+' $> (+), char '*' $> (*)]

parseOperand :: ReadP ((Int -> Int -> Int) -> Int -> Int)
parseOperand = choice [string "old" $> applyTwice, (\n f -> f n) <$> parseInt] where
    applyTwice op v = op v v

parseThrowTest :: ReadP (Int -> Int, Int)
parseThrowTest = do
    _ <- skipSpaces *> string "Test: "
    divisor <- parseTestCondition
    _ <- skipSpaces
    targetIfTrue <- string "If true: throw to monkey " *> parseInt
    _ <- skipSpaces
    targetIfFalse <- string "If false: throw to monkey " *> parseInt
    return (\x -> if x `mod` divisor == 0 then targetIfTrue else targetIfFalse, divisor)

parseTestCondition :: ReadP Int
parseTestCondition = string "divisible by " *> parseInt
