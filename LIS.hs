{-
I implemented the naïve approach to solve LIS. 
Furthermore, I tried to came up with an approach to reduce 
the complexity by implementing (Halving Quick Sort)
in which I tried to utilize the idea of choosing the first 
element of the list as the pivot in a Quick sort algorithm. 
However, such an approach failed to find all possible LIS.
-}


module Main where
import Data.List

main :: IO ()
main = do
-- Try your own sequance by changing this list elements
    let list = [6, 3, 5, 2, 7, 8, 1]
    -- Uncomment lines below to see all increasing subsequences 
    --putStrLn "All increasings are:"
    --print $   allincreasing list
    putStrLn "The elements in LIS are:"
    print $  longest $ allincreasing list
    putStrLn "The length of LIS is:"
    print $ nub $ len $ longest $ allincreasing list
    ---try Halving Quick Sort Approach
    --putStrLn "All increasing subsequances are:"
    --print $   halvingQuickSorts list
    putStrLn "Using HalvingQuickSort, LIS elemnts are:" 
    print $   longest $ halvingQuickSorts list
    putStrLn "The legnth of LIS is:"
    print $ nub $ len $ longest $ halvingQuickSorts list



-- Step 1 : Write a Recursive sorting function Recursively Check if a given subsequence is increasing 
    -- Where the input subsequence is each output of the high-order function subsequances in Step 2 

isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing []       = True
isIncreasing [x]      = True
isIncreasing (x:y:xs) = x <= y && isIncreasing (y:xs)

-- Step 2 : Find all increasing subseqances 
   -- The function "allincreasing" is defined with the help of dot operator used with high-order function 
   -- The Idea behind dot operator is to feed the output of the function on the right of the dot as input to the function on its left
   -- Accordingly, all possible subsequences outputed from "subsequences" function will be passed to the "filter" function which takes "isIncreasing" as an argument 
   -- Finally, dot operator is used again to find uniqe sequances only using "nub"

allincreasing :: Ord a => [a] -> [[a]]
allincreasing =  map nub  . filter isIncreasing . subsequences


-- Step 3: Find the longest increasing subsequence using Recursion and auxiliary function 

longest :: [[a]] -> [[a]]
longest lst = longest' lst [[]]

longest' :: [[a]]->[[a]]->[[a]] -- input, working state, output
longest' [] x = x --base case: if there's empty input return working state.
longest' (y:lst) x
 | length (head x) < length y = longest' lst [y]
 | length (head x) == length y = longest' lst (x++[y])
 | otherwise = longest' lst x


-- To print the length of LIS
len :: [[a]] -> [Int]
len (xs:xss) = length xs : len xss
len _        = []

-- A sort of Accumolator can be used with the recerusion to track max length alongsiede recursion 

-- However, it will only return on LIS in case if we have multiple ones with the same length 
{-
longest :: Ord a => [[a]] -> [a]
longest list = longest' list [] 
    where
        longest' [] acc = acc
        longest' (x:xs) [] = longest' xs x
        longest' (x:xs) acc 
            | length acc >= length x = longest' xs acc
            | length acc < length x = longest' xs x
        longest' _ _ = error "something went wrong..."

-}
 
 ----------------------------- Halving Quick Sort Approach  -----------------------

-- Step 1 :  Directly find increasing sequances 

halvingQuickSort :: Ord a => [a] -> [a]
halvingQuickSort [] = []
halvingQuickSort (p:xs) =  p : halvingQuickSort (filter (>= p) xs)

halvingQuickSorts :: Ord a => [a] -> [[a]]
halvingQuickSorts [] = []
halvingQuickSorts xa@(_:xs) = halvingQuickSort xa : halvingQuickSorts xs


-- Step 2: Find the longest increasing subsequence using Recursion and auxiliary function defined before 

