module Utils where
import System.IO.Unsafe ( unsafePerformIO )  -- be careful!                                          
import System.Random ( getStdRandom, Random(randomR) )

-- Removes an Eq item
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

mean:: [Float] -> Float
mean items = sum items / total
 where total = fromIntegral (length items) :: Float


-- Gets a random element in beetween fst and snd of Tuple. snd should be greater than
pickRandomInt:: (Int,Int) -> Int
pickRandomInt (m,n) = unsafePerformIO (getStdRandom (randomR (m, n)))

pickRandom :: [a] -> a 
pickRandom list
    | length list == 1 = head list
    | otherwise = list !!
    unsafePerformIO (getStdRandom (randomR (0, length list-1)))


manhattanDistance:: (Int,Int) -> (Int,Int) -> Int
manhattanDistance (m1,n1) (m2,n2) = abs (m1 - m2) + abs (n1 - n2)

-- F# pipe operator
(|>) :: a -> (a -> b) -> b
a |> b = b a


headSafe :: [a] -> Maybe a
headSafe []     = Nothing
headSafe (x:xs) = Just x
