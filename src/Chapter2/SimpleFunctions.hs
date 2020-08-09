module Chapter2.SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty lst =
  if not (null lst)
    then head lst
    else "empty"

(+++) :: String -> String -> String
lst1 +++ lst2 =
  if null lst1
    then lst2
    else (head lst1) : (tail lst1 +++ lst2)

maxmin :: [Int] -> (Int, Int)
maxmin list = let h = head list
              in if null (tail list)
                then (h, h)
                else (  if h > t_max then h else t_max
                      , if h < t_min then h else t_min)
                      where t = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t


myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x,y):pairs) = (x:xs, y:ys)
  where
    (xs, ys) = myUnzip pairs
