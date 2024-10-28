import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import qualified Data.Map
import qualified Data.Maybe

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

-- Uses a Dijkstra's algorithm to find all shortest paths from start to goal.
-- Arguments:
-- adjList: the adjacency list representing the graph.
-- start: the starting city.
-- goal: the destination city.
-- distances: a map that holds the shortest known distance from the start city to each city.
-- foundPaths: a list of completed paths from start to goal.
-- currentPath : remainingPaths: a list of paths that are still being explored, starting with the current path.
-- Returns: A list of paths, where each path is the shortest route from start to goal.
dijkstra :: AdjList -> City -> City -> Data.Map.Map City Distance -> [Path] -> [Path] -> [Path]
dijkstra adjList start goal distances foundPaths [] = foundPaths -- if the list of remaining paths is empty return accumulated list of found paths.
dijkstra adjList start goal distances foundPaths (currentPath : remainingPaths)
    -- if the goal is reached, compare the current path's distance with the shortest found so far.
    | currentCity == goal =
        let currentDist = pathTotalDistance currentPath adjList
            minDist = case Data.Maybe.listToMaybe foundPaths of
                        Nothing -> currentDist
                        Just fp -> pathTotalDistance fp adjList
        in if currentDist < minDist
            -- if the current path is shorter, replace the found paths with the current path.
            then dijkstra adjList start goal distances [currentPath] remainingPaths
            else if currentDist == minDist
                -- if the current path has the same distance, add it to the found paths.
                then dijkstra adjList start goal distances (currentPath : foundPaths) remainingPaths
                -- if the current path is longer, continue with the existing found paths.
                else dijkstra adjList start goal distances foundPaths remainingPaths
    -- if the goal is not reached, calculate new distances and paths, and sort paths to prioritize shorter ones.
    | otherwise = dijkstra adjList start goal newDistances foundPaths (Data.List.sortOn (\p -> pathTotalDistance p adjList) (remainingPaths ++ newPaths))
    where
        -- get the last city in the current path and retrieve its distance.
        currentCity = last currentPath
        currentDistance = Data.Maybe.fromMaybe maxBound (Data.Map.lookup currentCity distances)

        -- retrieve neighboring cities and distances from the adjacency list.
        neighbors = Data.Maybe.fromMaybe [] (lookup currentCity adjList)

        -- update distances and generate new paths by extending the current path to each neighbor.
        (newDistances, newPaths) = Data.List.foldl' updatePaths (distances, []) neighbors

        -- Helper function that updates distances and paths for each neighbor.
        -- Arguments:
        -- dists: the map of distances to be updated.
        -- paths: the list of new paths.
        -- (neighbor, weight): the current neighbor and the distance to it from the current city.
        updatePaths (dists, paths) (neighbor, weight) =
            let newDistance = currentDistance + weight
                oldDistance = Data.Maybe.fromMaybe maxBound (Data.Map.lookup neighbor dists)
            in if newDistance < oldDistance
                -- if a shorter distance is found, update the distance and add the new path.
                then (Data.Map.insert neighbor newDistance dists, (currentPath ++ [neighbor]) : paths)
                -- if the distance is the same as the known shortest, add the path without updating the distance.
                else if newDistance == oldDistance
                    then (dists, (currentPath ++ [neighbor]) : paths)
                    else (dists, paths)

        -- calculate the total distance of a path using the adjacency list.
        pathTotalDistance :: Path -> AdjList -> Distance
        pathTotalDistance path adjList = sum [Data.Maybe.fromMaybe 0 (lookupDistance (path !! (i - 1)) (path !! i) adjList) | i <- [1..length path - 1]]

        -- helper function to look up the distance between two cities in the adjacency list.
        lookupDistance :: City -> City -> AdjList -> Maybe Distance
        lookupDistance c1 c2 adjList = lookup c2 =<< lookup c1 adjList

-- Function to find the shortest path between two cities in a RoadMap.
-- Arguments:
-- roadMap: a list of tuples, where each tuple contains two cities and the distance between them.
-- start: the starting city.
-- goal: the destination city.
-- Returns: A list of paths, where each path is the shortest route from start to goal.
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start goal
    | null adjList = [] -- if the adjacency list is empty, return an empty list.
    | not (start `elem` map fst adjList) = [] -- if the starting city is not in the adjacency list, return an empty list.
    | otherwise = dijkstra adjList start goal initialDistances [] [[start]] -- start Dijkstra's with the start city.
    where
        -- convert the RoadMap to an adjacency list.
        adjList = roadMapToAdjList roadMap
        -- initialize the distances map with the start city at 0 distance, and all other cities at maxBound representing infinity.
        initialDistances = Data.Map.insert start 0 (Data.Map.fromList [(city, maxBound) | (city, _, _) <- roadMap])

-- ==================================================================

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

-- More graphs to test
gTest4 :: RoadMap
gTest4 = [("0","1",2),("0","2",3),("1","3",4),("2","3",3),("1","2",1)]

gTest5 :: RoadMap
gTest5 = [("0","1",6),("0","3",1),("1","2",5),("1","3",2),("1","4",2),("2","4",5),("3","4",1)]