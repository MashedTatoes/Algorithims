merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs)(y:ys) | x < y = x:(merge xs (y:ys))
                   | otherwise = y:(merge (x:xs) ys)
merge _ _ = []


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [xs] = [xs]
mergeSort xs =
  let len = div (length xs) 2
  in
    merge (mergeSort (take len xs) ) (mergeSort (take len $ drop len xs))

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y:(insert x ys)

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
