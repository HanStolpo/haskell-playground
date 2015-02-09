module Main where
import System.Environment (getArgs)
import Debug.Trace
import Data.List

fib_worst :: Int -> Integer
fib_worst 0 = 0
fib_worst 1 = 1
fib_worst 2 = 1
fib_worst n = fib_worst(n-2) + fib_worst(n-1)

data Fibber = Fibber {fibNum :: Int, fibValue :: Integer}
makeFibber :: Int -> Fibber
makeFibber a = Fibber a (fib_worst a)
instance Eq Fibber where a == b = fibNum a == fibNum b
instance Ord Fibber where a <= b = fibNum a <= fibNum b
instance Show Fibber where show a = show . fibNum $ a
instance Num Fibber where
    a + b = makeFibber (fibNum a + fibNum b)
    a * b = makeFibber (fibNum a * fibNum b)
    abs = makeFibber . abs . fibNum
    signum = makeFibber . signum . fibNum
    fromInteger = makeFibber . fromInteger
    negate = makeFibber . negate . fibNum

fib_worst' :: Int -> Integer
fib_worst' 0 = trace "fib_worst 0"  0
fib_worst' 1 = trace "fib_worst 1"  1
fib_worst' 2 = trace "fib_worst 2"  1
fib_worst' n = trace ("fib_worst " ++ show n) $ fib_worst'(n-2) + fib_worst'(n-1)

fib_bad :: Int -> Integer
fib_bad 0 = 0
fib_bad 1 = 1
fib_bad 2 = 1
fib_bad n = let n2 = fib_bad(n-2)
                n1 = fib_bad(n-1)
                in n2 `seq` (n1 `seq` (n2 + n1))

fib_bad' :: Int -> Integer
fib_bad' 0 = trace "fib_bad 0" 0
fib_bad' 1 = trace "fib_bad 1" 1
fib_bad' 2 = trace "fib_bad 2" 1
fib_bad' n = let n2 = fib_bad'(n-2)
                 n1 = fib_bad'(n-1)
                 in n2 `seq` (n1 `seq` (trace ("fib_bad " ++ show n) $ n2 + n1))

fib_good :: Int -> Integer
fib_good n = l !! n where l = 0:1:1:2:[n2 + n1| (n2, n1) <- zip (drop 2 l) (drop 3 l)]

fib_list :: [Integer]
fib_list = 0:1:1:2:[n2 + n1| (n2, n1) <- zip (drop 2 fib_list) (drop 3 fib_list)]
fib_best :: Int -> Integer
fib_best n = fib_list !! n

sum_big :: Int -> Integer
sum_big n = foldl (+) (fromIntegral n)   [1..10000000::Integer]

sum_big' :: Int -> Integer
sum_big' n = foldl' (+) (fromIntegral n) [1..10000000::Integer] 

sum_small :: Int -> Integer
sum_small n = trace "sum_10 called " $ foldl f (fromIntegral n) [1..10::Integer]
    where
        f a b = trace "sum_10 helper " (a + b)

sum_small' :: Int -> Integer
sum_small' n = trace "sum_10 called " $ foldl' f (fromIntegral n) [1..10::Integer]
    where
        f a b = trace "sum_10 helper " (a + b)


splitAt_lp' :: (String -> ([a],[a]) -> ([a],[a])) -> Int -> [a] -> ([a], [a])
splitAt_lp' tag n xs = tag ("splitAt_lp " ++ show n) $
    if n<=0
        then ([], xs)
        else
            case xs of
                [] -> ([], [])
                y:ys ->
                    case splitAt_lp' tag (n-1) ys of
                        ~(prefix, suffix) -> (y : prefix, suffix)

splitAt_lp :: Int -> [a] -> ([a], [a])
splitAt_lp = splitAt_lp' (\_ b -> b)

splitAt_sp' :: (String -> ([a],[a]) -> ([a],[a])) -> Int -> [a] -> ([a], [a])
splitAt_sp' tag n xs = tag ("splitAt_sp " ++ show n) $
    if n<=0
        then ([], xs)
        else
            case xs of
                [] -> ([], [])
                y:ys ->
                    case splitAt_sp' tag (n-1) ys of
                        (prefix, suffix) -> (y : prefix, suffix)

splitAt_sp :: Int -> [a] -> ([a], [a])
splitAt_sp = splitAt_sp' (\_ b -> b)

sum_5_splitAt_10000000_lp :: () -> Integer
sum_5_splitAt_10000000_lp _ = sum . take 5 . fst . splitAt_lp 10000000 $ repeat 1

sum_5_splitAt_10000000_sp :: () -> Integer
sum_5_splitAt_10000000_sp _ = sum . take 5 . fst . splitAt_sp 10000000 $ repeat 1

factLazy :: Int -> Int
factLazy 1 = trace "factLazy 1" 1
factLazy n = let fNMin1 = factLazy (n-1) in trace ("factLazy " ++ show n) (fNMin1 * n)

factEager :: Int -> Int
factEager 1 = trace "factLazy 1" 1
factEager n = let fNMin1 = factEager (n-1) in fNMin1 `seq` (trace ("factLazy " ++ show n) (fNMin1 * n))

-- a(n+1) = (a(n) + N/a(n)) / 2
--nextSqrtApprox :: Fractional a => a -> a -> a
nextSqrtApprox n x = (x + n/x) / 2
-- iterate f x == [x, f x, f (f x), ...]
--sqrtApprox :: Fractional a => a -> [a]
sqrtApprox n = iterate (nextSqrtApprox n) (n/2)
--within :: (Num a, Ord a) => a -> [a] -> a
within eps (a:b:bs) | abs(a-b) <= eps = b
                    | otherwise = within eps (b:bs)
--within _ _ = error "within called with too short a list"
withinSqrt eps n = within eps (sqrtApprox n)
-- relative :: (Fractional a, Ord a) => a -> [a] -> a
relative eps (a:b:bs) | abs(a-b) <= eps * abs b = b
                      | otherwise = relative eps (b:bs)
relativeSqrt eps n = within eps (sqrtApprox n)




main :: IO ()
main = do
    args <- getArgs
    let func = head args
        n = read . head . drop 1 $ args
    case func of
        "fib_worst" -> putStrLn $ "fib_worst " ++ show n ++ " = " ++ show (fib_worst n)
        "fib_bad" -> putStrLn $ "fib_bad " ++ show n ++ " = " ++ show (fib_bad n)
        "fib_good" -> putStrLn $ "fib_good " ++ show n ++ " = " ++ show (fib_good n)
        "sum_big" -> putStrLn $ "sum_big " ++ show n ++ " = " ++ show (sum_big n)
        "sum_big'" -> putStrLn $ "sum_big'' " ++ show n ++ " = " ++ show (sum_big' n)
        "split_at_strict" -> putStrLn $ "sum_5_splitAt_10000000_sp" ++ " = " ++ show (sum_5_splitAt_10000000_sp ())
        "split_at_lazy" -> putStrLn $ "sum_5_splitAt_10000000_lp" ++ " = " ++ show (sum_5_splitAt_10000000_sp ())
        _ -> error "unknown argument"
