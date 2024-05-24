{-
Needed for edge case handle. Implement eventually... 

-- swap rows in the event that the pivot is zero.
swap :: [a] -> Int -> Int -> [a]
swap [] _ _ = []
-- swap matrix 0 j = swapHead matrix j
swap matrix i j
    | i < j = swap matrx j i 
    | i == j = matrix
    | i > j = swap matrix j i 
swap (r:rs) i j = r : (swap rs (i-1) (j-1))

insertAt :: a -> Int -> [a] -> [a] 
insertAt x _ [] = [x] 
insertAt x 0 xs = x : xs 
insertAt x n (y:ys) = y : 

-- helper to swap if the head is zero...
swapHead :: [a] -> Int -> [a]
swapHead [] _ = []
swapHead (r:s:rs) j = s:r:rs
-}

{-
    Sources: 
- https://sites.millersville.edu/bikenaga/linear-algebra/solving-systems-of-linear-equations/solving-systems-of-linear-equations.pdf
- https://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
-}

type Row = [Double]
type Matrix = [Row]

{-
    ref is a function that performs row reduction on a matrix
    in order to get the matrix into row echelon form.

    The function takes in a matrix and an integer representing
    the index of the row to start the reduction process.

    1. It extracts the leading coefficient from the specified row
    
    2. Create a modified row where every element is divided by the 
    leading coefficient, this will result in the leading coefficient 
    to become 1.

    3. Calculates the modified elements for a row in the matrix after subtracting
    multiples of the modified row from it in order to eliminate coefficients below
    the leading coefficient. 
    
    - row - represents a row from the original matrix.
    - modifiedRow - modified version of the current row where each element is divided by the
    leading coefficient.
    - lambda function subtracts the product of an element from the modifiedRow and the element
    in the row corresponding to the rowIdx.
    - zipWith - results in a new list where each element represents the result of subtracting 
    a multiple of the modified row from the corresponding element in the original row. 
    - map - applying the multiplication of -1 makes it so the leading coefficient is 1 and not 
    -1 for back substitution later on. 
    
    4. Drops the first rows from the matrix and then applies subRowFactor to each of the remaining
    rows. 

    5. Selects the rows above the current row you are on. 

    6. Repeats until it runs out of rows and then the matrix should be in ref after concatenating. 

    Example Input: 
    INPUT: ref [[1,2,-4],[2,3,5]] 0
    OUTPUT: [[1.0,2.0,-4.0],[-0.0,1.0,-13.0]]
-}
ref :: Matrix -> Int -> Matrix
ref matrix rowIdx =
    let leadingCoefficient = matrix !! rowIdx !! rowIdx -- 1
        modifiedRow = map (\x -> x / leadingCoefficient) (matrix !! rowIdx) -- 2
        subRowFactor row = map (*(-1)) (zipWith (\a b -> b - a * (row !! rowIdx)) modifiedRow row) -- 3
        belowRows = map (subRowFactor) (drop (rowIdx + 1) matrix) -- 4
        aboveRows = take rowIdx matrix -- 5
    in aboveRows ++ [modifiedRow] ++ belowRows -- 6

{-
    The function applies gaussian elimination to transform a given matrix
    into row echelon form by recursively eliminating rows starting from the
    first row until all rows have been processed.

    1. Base case: empty matrix!

    2. Start process at row 0 for a given non-empty matrix.

    3. Check and see if index is gte the length of the matrix, if true, we
    are done.

    Otherwise, calculate the ref form of the matrix starting from current
    row using the above ref function. Then increments the index to move to 
    the next row. Lastly, call eliminateRows to check and see if we need to
    go again or if we are done.
-}
gaussElim :: Matrix -> Matrix
gaussElim [] = []
gaussElim matrix = eliminateRows matrix 0 where
    eliminateRows mat idx
        | idx >= length mat = mat -- we've reached the end, no more rows to eliminate
        | otherwise = let
                refMat = ref mat idx -- Convert the matrix into row echelon form starting from current row
                nextIdx = idx + 1
            in eliminateRows refMat nextIdx

-- THIS IS NOT MY FUNCTION.
-- FOR DEMO PURPOSES!
-- OTHERWISE THIS DOES NOT COMPILE AND THE DEMO WONT WORK...
{-
    Title: Solving systems of linear equations in Haskell
    Author: luckytoilet
    Date: 2010
    Availability: https://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
-}  
-- folding from the right, each step it substitutes in the corresponding solution and 
-- multiplies and subtracts to get the next solution, adding that to the solution list
substitute :: Matrix -> Row
substitute matrix = foldr next [last (last matrix)] (init matrix) where
    next row found = 
        let subpart = init $ drop (length matrix - length found) row
            solution = last row - sum (zipWith (*) found subpart)
        in solution : found

{-
    Inputs is a matrix and uses gaussElim to get the matrix into
    ref and then back substitute the last row to solve for the 
    variables.
-}
solve :: Matrix -> Row
solve matrix = substitute (gaussElim matrix)

-- Parses input string into a matrix.
parseInput :: String -> Matrix
parseInput input = read input

main :: IO ()
main = do
    putStrLn "Enter the path to the input file:"
    filePath <- getLine
    input <- readFile filePath
    let matrix = parseInput input
        solution = solve matrix
    putStrLn "Solution:"
    print solution
    -- [NaN, NaN] is printed in the result of infinitely many solutions.
    -- [Infinity, Infinity] is print in the event of no solution...?