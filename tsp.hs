-- This is what is called when the compiled code is ran using "main" in GHCi
main = print $
    getRouteToString (minimum (map (getAllDistances (0, [])) (map appendStoke (polyPerms [0,1,2,3,4,5,6,7,8]))))
    
-- BRUTE FORCE CALLS:
-- getRouteToString (minimum (map (getAllDistances (0, [])) (map appendStoke (perms [0,1,2,3,4,5,6,7,8]))))
-- getRouteToString (minimum (map (getAllDistances (0, [])) (map appendStoke (polyPerms [0,1,2,3,4,5,6,7,8]))))

-- Converted to Floats
distances = [[0.0,129.0,119.0,43.6,98.6,98.6,86.3,52.2,85.3,44.5],
            [129.0,0.0,88.3,149.0,152.0,57.4,55.4,141.0,93.3,86.3],
            [119.0,88.3,0.0,97.4,71.6,72.6,42.5,71.6,35.5,92.1],
            [43.6,149.0,97.4,0.0,54.0,119.0,107.0,28.0,64.2,60.7],
            [98.6,152.0,71.6,54.0,0.0,138.0,85.2,39.9,48.6,90.7],
            [98.6,57.4,72.6,119.0,138.0,0.0,34.9,111.0,77.1,56.3],
            [86.3,55.4,42.5,107.0,85.2,34.9,0.0,80.9,37.9,44.7],
            [52.2,141.0,71.6,28.0,39.9,111.0,80.9,0.0,38.8,52.4],
            [85.3,93.3,35.5,64.2,48.6,77.1,37.9,38.8,0.0,47.4],
            [44.5,86.3,92.1,60.7,90.7,56.3,44.7,52.4,47.4,0.0]]

--Index:       0             1          2          3           4           5            6              7            8          9
cities = ["Birmingham", "Lancaster", "Leeds", "Leicester", "Lincoln", "Liverpool", "Manchester", "Nottingham", "Sheffield", "Stoke"]

-- Returns the name of the city with the corresponding index, provided index is valid
getCityName :: Int -> [Char]
getCityName n
	| n >= 0 && n <= 9 = cities !! n
	| otherwise = "Invalid city index"


-- Returns distance using 2 Ints, unless index is invalid in which case returns 99999
getDistance :: Int -> Int -> Float
getDistance x y
	| x >= 0 && x <= 9 && y >= 0 && y <= 9 = distances !! x !! y
	| otherwise = 99999


-- Returns distance using a tuple, unless index is invalid in which case returns 99999
getDistanceT :: (Int, Int) -> Float
getDistanceT (x, y)
	| x >= 0 && x <= 9 && y >= 0 && y <= 9 = distances !! x !! y
	| otherwise = 99999


-- Generates a list of all the permutations in lexicographic order for the input list
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms x = [ item:rest | item <- x, rest <- perms (searchAndDestroyOnce item x) ]


-- Polymorphic version of perms
polyPerms :: Eq a => [a] -> [[a]]
polyPerms [] = [[]]
polyPerms x = [ item:rest | item <- x, rest <- polyPerms (searchAndDestroyOnce item x) ]


-- Removes all instances of the item in a List and returns result
-- This is okay as they won't be duplicates.
-- Logic:
-- If matched, continue searching the remainder of the list
-- If not matched, append the item to the start of the List, then keep searching
searchAndDestroyAll :: Eq a => a -> [a] -> [a]
searchAndDestroyAll _ [] = []
searchAndDestroyAll item (x:xs)
	| item == x = searchAndDestroyAll item xs
	| otherwise = x : searchAndDestroyAll item xs


-- Optimised version of searchAndDestroyAll - returns after removing once
searchAndDestroyOnce :: Eq a => a -> [a] -> [a]
searchAndDestroyOnce _ [] = []
searchAndDestroyOnce item (x:xs)
	| item == x = xs
	| otherwise = x : searchAndDestroyOnce item xs


-- Adds stoke to the start and end of a list (used after permutations)
appendStoke :: [Int] -> [Int]
appendStoke [] = []
appendStoke xs = 9 : xs ++ [9]


-- Gets the total cost of a route by adding each distance until the end of the list is reached
-- First call should have an empty list in the tuple so that the route can be added to it before recursive call
getAllDistances :: (Float, [Int]) -> [Int] -> (Float, [Int])
getAllDistances (_,[_]) [] = (99999, [])
getAllDistances (_,[_]) [_] = (99999, [])
getAllDistances (cost,[]) (x:xs) = getAllDistances (cost, (x:xs)) (x:xs)
getAllDistances (cost, route) (first:second)
	| length second == 1 = (cost + getDistanceT (first, head second), route)
	| length second >= 2 = getAllDistances (updatedCost, route) second
	where updatedCost = getDistanceT (first, head second) + cost


-- Takes a cost and route and prints them in a tuple, but replaces cities as ints to their string values
getRouteToString :: (Float, [Int]) -> (Float, [[Char]])
getRouteToString (_, []) = (99999, [])
getRouteToString (cost, route) = (cost, map getCityName route)