import Constraint (Constraint(..), solve, binaryConstraint, notEqual, equalDoms)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import System.IO

data Operation = Add | Multiply | Subtract | Divide

gridVars size = [(x, y) | x <- [1..size], y <- [1..size]]
neConstraints size = do
  a <- [1..size]
  (notEqual [(a, i) | i <- [1..size]]) ++ (notEqual [(i, a) | i <- [1..size]])

mc :: Operation -> Int -> [v] -> Constraint v Int
mc op value vars =
  case op of
    Add -> Constraint { vars=vars, predicate=((==) value . foldl (+) 0) }
    Multiply -> Constraint { vars=vars, predicate=((==) value . foldl (*) 1) }
    Subtract -> binaryConstraint vars (\x y -> x + value == y || y + value == x)
    Divide -> binaryConstraint vars (\x y -> x * value == y || y * value == x)

parseConstraint :: [v] -> String -> Constraint v Int
parseConstraint vars line =
  let op = head line
      value = read (tail line) :: Int
  in
  let
    op' = case op of
      '+' -> Add
      '*' -> Multiply
      '-' -> Subtract
      '/' -> Divide
  in
  mc op' value vars

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

main :: IO ()
main = do
  putStrLn "Enter grid groups"
  firstLine <- getLine
  let size = length firstLine
      myVars = gridVars size in do
  lines <- sequence ((return firstLine) : [getLine | i <- [2..size]])
  let
    (groupOrder, groups) =
      foldl (\(go, gs) (i, line) ->
        foldl (\(go, gs) (j, group) ->
          let go' = if Map.member group gs then go else group : go in
          (go', Map.insertWith (++) group [(i, j)] gs)
        ) (go, gs) (zip [1..] line)
      ) ("", Map.empty) (zip [1..] lines)
    in do
    opConstraints <- sequence [fmap (parseConstraint (groups ! v)) (prompt (v:": ")) | v <- reverse groupOrder]
    let solution = solve myVars (equalDoms myVars [1..size]) (neConstraints size ++ opConstraints)
        solutionLines = group size $ map (head . show) solution
      in do
        putStrLn ""
        mapM_ putStrLn solutionLines
