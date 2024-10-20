import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Returns all unique cities in the RoadMap.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
cities :: RoadMap -> [City]
cities roadMap = uniqueCities
    where
        allCities = concat [[city1, city2] | (city1, city2, _) <- roadMap]  -- extract all cities from the RoadMap and concat to a list.
        uniqueCities = Data.List.nub allCities                              -- remove duplicates from the list.

-- Checks if two cities are directly connected.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- city1, city2: the names of the cities to check for adjacency.
areAdjacent :: RoadMap -> City -> City -> Bool
-- check if there's any tuple (c1, c2, _) in the RoadMap that confirms the cities are adjacent.
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

-- Returns the distance between two cities if they are directly connected, or Nothing if they are not.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- city1, city2: the names of the cities between which to calculate the distance.
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing                                       -- base case: if the RoadMap is empty, return Nothing.
distance ((c1, c2, dist):rest) city1 city2
    | areAdjacent [(c1, c2, dist)] city1 city2 = Just dist      -- if the cities are adjacent, return the distance.
    | otherwise = distance rest city1 city2                     -- otherwise, recursively check the rest of the RoadMap.

-- Returns a list of cities that are directly connected to the given city, along with their respective distances.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- originCity: the city for which to find adjacent cities.
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []                                              -- base case: if the RoadMap is empty, return an empty list.
adjacent ((c1, c2, dist):rest) originCity
    | originCity == c1 = (c2, dist) : adjacent rest originCity  -- if the originCity matches c1, add (c2, dist) to the result.
    | originCity == c2 = (c1, dist) : adjacent rest originCity  -- if the originCity matches c2, add (c1, dist) to the result.
    | otherwise = adjacent rest originCity                      -- otherwise, recursively check the rest of the RoadMap.

-- Calculates the total distance for a given path of cities. Returns Nothing if any cities in the path are not connected.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- path: a list of cities representing the path.
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Nothing         -- base case: if the path is empty, return Nothing.
pathDistance _ [_] = Just 0         -- base case: if the path has only one city, return Just 0.
pathDistance roadMap (city1:city2:rest) =
    -- check if city1 and city2 have a path in between using the 'distance' function
    case distance roadMap city1 city2 of
        -- if they are not directly connected, return Nothing.
        Nothing -> Nothing  
        -- if they are directly connected, get the distance.
        Just dist -> case pathDistance roadMap (city2:rest) of     -- recursively calculate the distance for the rest of the path.
            -- if the rest of the path is not valid, return Nothing.
            Nothing -> Nothing
            -- if the rest of the path is valid, sum the distance of the current path with the rest of the path.
            Just restDist -> Just (dist + restDist)

-- Finds the cities with the highest degree (most directly connected roads) in the RoadMap.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
rome :: RoadMap -> [City]
-- construct a list of cities by iterating over 'cityDegrees', which is a list of tuples (city, degree), and applying the equality degree == maxDegree
rome roadMap = [city | (city, degree) <- cityDegrees, degree == maxDegree]
    where
        -- extract all unique cities from the RoadMap using the 'cities' function.
        uniqueCities = cities roadMap   
        -- create a list of tuples (city, degree) where 'degree' is the number of roads connected to 'city'.
        cityDegrees = [(city, length (filter (\(c1, c2, _) -> c1 == city || c2 == city) roadMap)) | city <- uniqueCities]
        -- find the maximum degree from 'cityDegrees' by mapping 'snd' (second element of the tuple) and applying 'maximum'.
        maxDegree = maximum (map snd cityDegrees)

-- Auxiliar function that performs a depth-first search starting from a given city, returning a list of visited cities.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- originCity: the starting city for the search.
-- visitedCities: a list of cities that have already been visited.
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadMap originCity visitedCities
    | originCity `elem` visitedCities = visitedCities                   -- base case: if the originCity is already in the visitedCities list, return the visitedCities list.
    | otherwise = foldl auxVisit (originCity : visitedCities) roadMap   -- otherwise, add the originCity to the visitedCities list and visit connected cities.
    where
        -- auxiliar function to visit connected cities.
        auxVisit acc (c1, c2, _)
            | c1 == originCity = dfs roadMap c2 acc     -- if c1 is the originCity, recursively call dfs with c2.
            | c2 == originCity = dfs roadMap c1 acc     -- if c2 is the originCity, recursively call dfs with c1.
            | otherwise = acc                           -- if neither c1 nor c2 is the originCity, return the accumulator.

-- Checks if all cities in the RoadMap are strongly connected, meaning every city is reachable from every other city.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
isStronglyConnected :: RoadMap -> Bool
-- check if the length of the list of cities visited by 'dfs' starting from each city is equal to the length of 'uniqueCities'.
isStronglyConnected roadMap = all (\city -> length (dfs roadMap city []) == length uniqueCities) uniqueCities
    where
        -- extract all unique cities from the RoadMap using the 'cities' function.
        uniqueCities = cities roadMap

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]