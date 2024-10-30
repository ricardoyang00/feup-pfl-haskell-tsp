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

-- ==================================================================

-- Type alias for an adjacency list representation of a graph.
type AdjList = [(City, [(City, Distance)])]

-- Converts a RoadMap to an adjacency list representation.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
roadMapToAdjList :: RoadMap -> AdjList
roadMapToAdjList [] = [] -- if the RoadMap is empty, return an empty list.
roadMapToAdjList ((c1, c2, dist):rest) = 
    let adjList = roadMapToAdjList rest                 -- recursively process the rest of the roadmap.
    in addEdge c1 c2 dist (addEdge c2 c1 dist adjList)  -- add both directions of the road to the adjacency list.

-- Adds an edge between two cities to the adjacency list
-- If city1 is already in the list, it adds city2 as a neighbor with the distance
-- Otherwise, it creates a new entry for city1
-- Arguments:
-- city1: the city we are adding or updating in the adjacency list.
-- city2: the neighbor city connected to city1.
-- dist: the distance between city1 and city2.
-- ((city, neighbors):rest): the current adjacency list, where the head is a tuple (city, neighbors) representing a city and its list of neighbors, and rest is the rest of the adjacency list.
addEdge :: City -> City -> Distance -> AdjList -> AdjList
addEdge city1 city2 dist [] = [(city1, [(city2, dist)])]  -- if the list is empty, create the first entry.
addEdge city1 city2 dist ((city, neighbors):rest)
    | city == city1 = (city, (city2, dist) : neighbors) : rest         -- if city1 is already in the list, add city2 to its neighbors.
    | otherwise = (city, neighbors) : addEdge city1 city2 dist rest    -- otherwise, keep looking in the rest of the list.

-- Checks if a city is present in the road map.
-- Arguments:
-- city: the city to search for in the road map.
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
isCityInRoadMap :: City -> RoadMap -> Bool
isCityInRoadMap city roadMap = any (\(c1, c2, _) -> c1 == city || c2 == city) roadMap

-- Finds all shortest paths between two cities in the road map.
-- The function first checks if the road map and the specified cities are valid. 
-- If they are valid, it initializes a breadth-first search (BFS) to explore all paths from the start city to the goal city, collecting paths that match the shortest distance.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- start: the city from which to start the path search.
-- goal: the city at which to end the path search.
-- Returns: a list of paths (each path is a list of cities) from the start city to the goal city.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start goal
    -- if the road map, start city, or goal city is empty, return an empty list.
    | null roadMap || null start || null goal = []
    -- if the start or goal city is not in the road map, return an empty list.
    | not (isCityInRoadMap start roadMap) || not (isCityInRoadMap goal roadMap) = []
    -- otherwise, proceed with the BFS algorithm to find all shortest paths.
    | otherwise = bfs initialQueue [] maxBound []
        where
            -- convert the road map to an adjacency list.
            adjList = roadMapToAdjList roadMap
            -- initialize the BFS queue with the start city, an empty path, and a distance of 0.
            initialQueue = [(start, [], 0)]

            -- Breadth-First Search to find all shortest paths from the start city to the goal city.
            -- Processes a queue of cities with their corresponding paths and distances, exploring routes while tracking the minimum distance and associated paths.
            -- Arguments:
            -- queue: list of tuples containing the current city, path, and distance.
            -- visited: list of already visited cities to prevent cycles.
            -- minDist: minimum distance found to the goal city.
            -- paths: accumulated list of shortest paths to the goal city.
            -- Returns: list of paths from the start city to the goal city.
            bfs :: [(City, [City], Distance)] -> [City] -> Distance -> [Path] -> [Path]
            -- if the queue is empty, return the collected paths.
            bfs [] _ _ paths = paths
            -- deconstruct the first element of the queue into currentCity, path, and currentDist.
            bfs ((currentCity, path, currentDist):rest) visited minDist paths
                -- if the current city is the goal city:
                | currentCity == goal =
                    let newPaths = if currentDist < minDist
                                    -- if the current distance is less than the minimum distance found so far, start a new list of paths with the current path.
                                    then [reverse (currentCity : path)]
                                    -- if the current distance is equal to the minimum distance, add the current path to the list of paths.
                                    else if currentDist == minDist
                                        then reverse (currentCity : path) : paths
                                        -- otherwise, keep the existing paths.
                                        else paths
                    -- continue BFS with the rest of the queue, updating the minimum distance and paths.
                    in bfs rest visited (min minDist currentDist) newPaths
                -- if the current city is not the goal city:
                | otherwise =
                    -- get the neighbors of the current city from the adjacency list.
                    let neighbors = case lookup currentCity adjList of
                            Just ns -> ns
                            Nothing -> []
                        -- create new paths by adding each neighbor to the current path, and updating the distance.
                        newPaths = [(nextCity, currentCity : path, currentDist + dist) | (nextCity, dist) <- neighbors, not (nextCity `elem` visited)]
                    -- continue BFS with the rest of the queue and the new paths, sorting the queue by distance to prioritize shorter paths.
                    in bfs (Data.List.sortOn (\(_, _, d) -> d) (rest ++ newPaths)) (currentCity : visited) minDist paths

-- ==================================================================

type CityIndex = Int
type AdjMatrix = Data.Array.Array (CityIndex, CityIndex) (Maybe Distance)

-- Function to associate each city with a unique index
-- e.g. [("A", 0), ("B", 1), ("C", 2), ...]
uniqueCitiesWithIndices :: RoadMap -> ([(City, CityIndex)], Int)
uniqueCitiesWithIndices roadMap =
    let uniqueCities = cities roadMap
        cityIndexPairs = zip uniqueCities [0..]  -- Create pairs of (City, Index)
    in (cityIndexPairs, length uniqueCities) 

-- Convert a city to its index
cityToIndex :: [(City, CityIndex)] -> City -> CityIndex
cityToIndex cityIndices city = 
    case lookup city cityIndices of
        Just index -> index
        Nothing -> error $ "City not found: " ++ city

-- Convert an index back to a city
indexToCity :: [(City, CityIndex)] -> CityIndex -> City
indexToCity cityIndices index = 
    case lookup index (map swap cityIndices) of
        Just city -> city
        Nothing -> error $ "Index not found: " ++ show index
    where swap (x, y) = (y, x)

-- Convert RoadMap to AdjMatrix
-- type AdjMatrix = Array (CityIndex, CityIndex) (Maybe Distance)
roadMapToAdjMatrix :: RoadMap -> AdjMatrix
roadMapToAdjMatrix roadMap =
    let (cityIndices, numCities) = uniqueCitiesWithIndices roadMap
        bounds = ((0, 0), (numCities - 1, numCities - 1))
        entries = [((cityToIndex cityIndices c1, cityToIndex cityIndices c2),
                    if c1 == c2 then Just 0
                    else distance roadMap c1 c2) |
                   c1 <- map fst cityIndices, c2 <- map fst cityIndices]
    in Data.Array.array bounds $ map (\((i, j), d) -> ((i, j), d)) entries ++ 
                                  map (\((j, i), d) -> ((j, i), d)) entries

-- Debug function to print the adjacency matrix
printAdjMatrix :: RoadMap -> IO ()
printAdjMatrix roadMap = do
    let adjMatrix = roadMapToAdjMatrix roadMap
        (cityIndices, numCities) = uniqueCitiesWithIndices roadMap
        cities = map fst cityIndices
    putStrLn "Adjacency Matrix:"
    putStrLn $ "   " ++ unwords (map show [0..numCities-1])
    mapM_ (\i -> do
        putStr $ show i ++ "  "
        mapM_ (\j -> 
                let distanceValue = adjMatrix Data.Array.! (i, j)
                in case distanceValue of
                    Just d  -> putStr $ show d ++ " "   -- Print distance if available
                    Nothing -> putStr $ "- "           -- Print "N" (∞) if there's no connection
            ) [0..numCities-1]
        putStrLn ""
        ) [0..numCities-1]


-- Solve the Traveling Salesman Problem (TSP) using dynamic programming
travelSales :: RoadMap -> Path
travelSales roadMap =
    let
        -- Map each unique city to a unique index and get the total number of cities (n)
        (cityIndices, n) = uniqueCitiesWithIndices roadMap

        -- Convert the road map to an adjacency matrix represented as an array
        distArray = roadMapToAdjMatrix roadMap

        -- Memoization table:
        -- dp[currentCity][visitedSet] = (cost, nextCity)
        -- - currentCity: The index of the current city in the tour
        -- - visitedSet: A bitmask representing the set of cities visited so far
        -- - cost: The minimal cost to complete the tour from currentCity
        -- - nextCity: The next city to visit from currentCity
        -- Initialize the memoization table with Nothing for all states
        dp :: Data.Array.Array (Int, Int) (Maybe (Distance, Int))
        dp = Data.Array.listArray 
                ((0, 0), (n-1, (1 `Data.Bits.shiftL` n) - 1))  -- Bounds: (0,0) to (n-1, 2^n - 1)
                (repeat Nothing)  -- Initialize all entries to Nothing

        -- Recursive TSP function with memoization
        -- Parameters:
        -- - currentCity: The current city's index
        -- - visited: Bitmask of visited cities
        -- - dpMemo: Current state of the memoization table
        -- Returns:
        -- - (Distance, Updated Memoization Table)
        tsp :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Int)) -> (Distance, Data.Array.Array (Int, Int) (Maybe (Distance, Int)))
        tsp currentCity visited dpMemo
            -- Base case: All cities have been visited
            | visited == (1 `Data.Bits.shiftL` n) - 1 =
                case distArray Data.Array.! (currentCity, 0) of  -- Distance from currentCity back to start (city 0)
                    Just d  -> (d, dpMemo)  -- If a return path exists, return its distance
                    Nothing -> (maxBound :: Int, dpMemo)  -- If no return path, assign maximum bound as cost
            -- Recursive case: Visit the next unvisited city
            | otherwise =
                case dpMemo Data.Array.! (currentCity, visited) of
                    Just (cost, _) -> (cost, dpMemo)  -- If the state is already computed, return the stored cost
                    Nothing ->
                        -- Explore all possible next cities and find the one with minimal total cost
                        let
                            -- Fold over all possible next city indices to find the minimal cost
                            (minCost, dpUpdated) = foldl nextCity (maxBound :: Int, dpMemo) [0..n-1]
                            
                            -- Helper function to evaluate each possible next city
                            nextCity (currentMin, dpAcc) nextCityIdx
                                -- Skip if nextCityIdx has already been visited
                                | visited Data.Bits..&. (1 `Data.Bits.shiftL` nextCityIdx) /= 0 = (currentMin, dpAcc)
                                | otherwise =
                                    case distArray Data.Array.! (currentCity, nextCityIdx) of
                                        Just d ->
                                            let
                                                -- Mark the next city as visited
                                                visited' = visited Data.Bits..|. (1 `Data.Bits.shiftL` nextCityIdx)
                                                
                                                -- Recursively compute the cost from nextCityIdx with the updated visited set
                                                (costNext, dpNext) = tsp nextCityIdx visited' dpAcc
                                                
                                                -- Total cost if visiting nextCityIdx
                                                totalCost = d + costNext
                                                
                                                -- Update the minimal cost and memoization table if a better path is found
                                                (newMin, dpNew) = if totalCost < currentMin
                                                    then (totalCost, dpNext Data.Array.// [((currentCity, visited), Just (totalCost, nextCityIdx))])
                                                    else (currentMin, dpNext)
                                            in (newMin, dpNew)
                                        Nothing -> (currentMin, dpAcc)  -- If no direct path, skip
                        in (minCost, dpUpdated)  -- Return the minimal cost and the updated memoization table

        -- Function to reconstruct the optimal path from the memoization table
        -- Parameters:
        -- - currentCity: The current city's index
        -- - visited: Bitmask of visited cities
        -- - dpMemo: The final state of the memoization table after computation
        -- Returns:
        -- - [City]: The list of cities representing the optimal path
        reconstructPath :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Int)) -> [City]
        reconstructPath currentCity visited dpMemo
            -- Base case: All cities have been visited, return to the starting city
            | visited == (1 `Data.Bits.shiftL` n) - 1 =
                [indexToCity cityIndices currentCity, indexToCity cityIndices 0]
            -- Recursive case: Follow the nextCity pointers from the memoization table
            | otherwise =
                case dpMemo Data.Array.! (currentCity, visited) of
                    Just (_, nextCityIdx) ->
                        -- Prepend the current city to the path and recurse with the next city marked as visited
                        indexToCity cityIndices currentCity : reconstructPath nextCityIdx (visited Data.Bits..|. (1 `Data.Bits.shiftL` nextCityIdx)) dpMemo
                    Nothing -> []  -- If no path exists, return an empty list

        -- Initialize the visited set with the starting city (assumed to be city 0)
        initialVisited = 1 `Data.Bits.shiftL` 0  -- Binary: 000...1
        
        -- Execute the TSP function starting from city 0 with the initial visited set
        (minCost, dpFinal) = tsp 0 initialVisited dp
    in
        if minCost == (maxBound :: Int) 
            then []
            else
                -- Reconstruct and return the optimal path if a solution exists
                let path = reconstructPath 0 initialVisited dpFinal
                in path

-- ==================================================================

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- More graphs to test
gTest4 :: RoadMap
gTest4 = [("0","1",6),("0","3",1),("1","2",5),("1","3",2),("1","4",2),("2","4",5),("3","4",1)]

gTest5 :: RoadMap
gTest5 = [("0","1",2),("0","2",3),("1","3",4),("2","3",3),("1","2",1)]
