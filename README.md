# T06_G05
- Bruno Huang (up202207517) - 50%
- Ricardo Yang (up202208465) - 50%

## Brief description of the tasks each one performed

We collaborated on the first seven questions, exchanging ideas, and then Bruno tackled the shortest path problem while Ricardo worked on the traveling salesman problem.

## Shortest Path Function

1. Data Structure Selection and Conversion

    **RoadMap to Adjacency List Conversion**

    The given RoadMap format is a list of tuples where each tuple represents a road between two cities and includes the distance. However, for efficient pathfinding, this format is converted into an adjacency list. An adjacency list provides a more direct representation of each city’s neighbors and their respective distances, making it more efficient for accessing neighboring cities during path exploration.

    ```hs
    AdjList = [(City, [(City, Distance)])]
    ```
    - It allows direct access to each city’s neighbors, essential for traversing paths.
    - It improves readability and efficiency compared to repeatedly scanning through a RoadMap for neighbors.

    <ins>roadMapToAdjList</ins>:
    
    The ***roadMapToAdjList*** function converts the **RoadMap** to an **AdjList** by iterating over each road in the roadmap and adding the connection between two cities (both directions) to create a bidirectional adjacency list.

    ```hs
    roadMapToAdjList :: RoadMap -> AdjList
    roadMapToAdjList [] = []
    roadMapToAdjList ((c1, c2, dist):rest) = 
        let adjList = roadMapToAdjList rest
        in addEdge c1 c2 dist (addEdge c2 c1 dist adjList)
    ```

    <ins>addEdge</ins>

    The ***addEdge*** function is a helper that adds a direct connection from one city to another with a specified distance. It appends city2 as a neighbor of city1 if city1 is already in the adjacency list. If city1 is not in the list, it creates a new entry with city2 as its neighbor.

    ```hs
    addEdge :: City -> City -> Distance -> AdjList -> AdjList
    addEdge city1 city2 dist [] = [(city1, [(city2, dist)])]
    addEdge city1 city2 dist ((city, neighbors):rest)
        | city == city1 = (city, (city2, dist) : neighbors) : rest
        | otherwise = (city, neighbors) : addEdge city1 city2 dist rest 
    ```

2. Finding the Shortest Path Using Breadth-First-Search (BFS)

    The ***shortestPath*** function uses a modified Breadth-First Search (BFS) to find all shortest paths between the starting and goal cities. BFS is well-suited for this task because:

    - It explores paths level by level, ensuring that the shortest path (in terms of  distance) is found first.
    - It systematically examines each possible path by increasing distance, which allows  it to efficiently discover the shortest routes without getting stuck in cycles.

    Before searching, ***shortestPath*** confirms that:

    - The **roadMap** is not empty.
    - Both the **start** and **goal** cities are specified and exist within the roadMap. If these checks fail, the function returns an empty list to indicate that no path can be found.

    To check this second point a helper function is used:

    ```hs
    isCityInRoadMap :: City -> RoadMap -> Bool
    isCityInRoadMap city roadMap = any (\(c1, c2, _) -> c1 == city || c2 == city) roadMap
    ```

    Then the function transforms the **roadMap** into an **adjList**

    The BFS-based approach processes each city layer by layer to guarantee the shortest paths (in terms of distance) are identified first. Each queue entry (as we couldn't import any other data types than the given ones we used a list to represent the bfs queue) includes the **<ins>current city</ins>**, the **<ins>accumulated path</ins>**, and the **<ins>current total distance</ins>**. This iterative process builds up the path from start to goal as follows:

    ```hs
    shortestPath :: RoadMap -> City -> City -> [Path]
    shortestPath roadMap start goal
        | null roadMap || null start || null goal = []
        | not (isCityInRoadMap start roadMap) || not (isCityInRoadMap goal roadMap) = []
        | otherwise = bfs initialQueue [] maxBound []
        where
            adjList = roadMapToAdjList roadMap
            initialQueue = [(start, [], 0)]
            
            bfs :: [(City, [City], Distance)] -> [City] -> Distance -> [Path] -> [Path]
            bfs [] _ _ paths = paths
            bfs ((currentCity, path, currentDist):rest) visited minDist paths
                | currentCity == goal =
                    let newPaths = if currentDist < minDist
                                    then [reverse (currentCity : path)]
                                    else if currentDist == minDist
                                        then reverse (currentCity : path) : paths
                                        else paths
                    in bfs rest visited (min minDist currentDist) newPaths
                | otherwise =
                    let neighbors = case lookup currentCity adjList of
                            Just ns -> ns
                            Nothing -> []
                        newPaths = [(nextCity, currentCity : path, currentDist + dist) | (nextCity, dist) <- neighbors, not (nextCity `elem` visited)]
                    in bfs (Data.List.sortOn (\(_, _, d) -> d) (rest ++ newPaths)) (currentCity : visited) minDist paths
    ```
    
    - **Step 1:** Start from the start city, initializing the queue and visiting cities in layers.

    - **Step 2:** When reaching the goal city, compare the current path's distance to the shortest path found so far. If it’s shorter, update the shortest path; if equal, add it as an alternative shortest path.

    - **Step 3:** If not yet at the goal, continue exploring neighboring cities, adding new paths with updated distances to the queue.

    - **Cycle Avoidance:** Tracks visited cities to avoid reprocessing cities, keeping each path unique and acyclic.

    **⏱️ Time Complexity:** O(V + E log E)

    **🚀 Space Complexity:** O(V + E)

## Travel Sales Function

1. Data Structure Selection and Conversion

    **RoadMap to Adjacency Matrix Conversion**
    To achieve more efficient solutions, it is highly recommended to convert the graph to a more convenient representation.

    A good representation is the Adjacency Matrix, which consists of a two-dimensional array where each element represents the distance between two cities. If there is no direct path between two cities, the corresponding element is `Nothing`.

    ```hs
    type CityIndex = Int
    type AdjMatrix = Data.Array.Array (CityIndex, CityIndex) (Maybe Distance)
    ```
    - The primary advantage is that it allows constant time complexity O(1) for edge lookups.

    <ins>roadMapToAdjMatrix</ins>:

    The `roadMapToAdjMatrix` function converts a RoadMap to an adjacency matrix. It first assigns each city a unique index using the `uniqueCitiesWithIndices` function, which returns a list of pairs (City, CityIndex) and the total number of unique cities (n).

    ```hs
    uniqueCitiesWithIndices :: RoadMap -> ([(City, CityIndex)], Int)
    uniqueCitiesWithIndices roadMap =
        let uniqueCities = cities roadMap
            cityIndexPairs = zip uniqueCities [0..]
        in (cityIndexPairs, length uniqueCities) 
    ```

    The roadMapToAdjMatrix function then creates the adjacency matrix by mapping each city pair to their corresponding indices and distances.

    > Note: Since all graphs are undirected, the adjacency matrix is also symmetric.

    ```hs
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
    ```

    Additionally, two helper functions are provided to convert between cities and their corresponding indices.

    ```hs
    cityToIndex :: [(City, CityIndex)] -> City -> CityIndex
    cityToIndex cityIndices city = 
        case lookup city cityIndices of
            Just index -> index
            Nothing -> error $ "City not found: " ++ city
    
    indexToCity :: [(City, CityIndex)] -> CityIndex -> City
    indexToCity cityIndices index = 
        case lookup index (map swap cityIndices) of
            Just city -> city
            Nothing -> error $ "Index not found: " ++ show index
        where swap (x, y) = (y, x)
    ```

2. Solving the TSP using Dynamic Programming

    The Traveling Salesman Problem (TSP) aims to find the least-cost path that visits each city exactly once and returns to the starting point. 
    
    The dynamic programming approach, known as the Held-Karp algorithm, addresses this by breaking the problem into smaller sub-problems and reusing their solutions to build an global solution. This approach significantly reduces the number of computations, making it a more practical solution for solving TSP.

    ### Brute force VS DP
    |n|Brute Force (n!)|DP (n<sup>2</sup> 2<sup>n</sup>)|
    |-|-|-|
    |1|1|2|
    |2|2|16|
    |3|6|72|
    |4|24|256|
    |5|120|800|
    |6|720|2304|
    |…|…|…|
    |15|1307674368000|7372800|
    |16|20922789888000|16777216|
    |17|355687428096000|37879808|

    ### DP Algorithm Explanation

    1. **Data Structure Selection and Conversion:**
    - The function starts by mapping each unique city to a unique index and getting the total number of cities (`n`).
    - It then converts the RoadMap to an adjacency matrix (`m`) using `roadMapToAdjMatrix` function.

    ```hs
    (cityIndices, n) = uniqueCitiesWithIndices roadMap
    m = roadMapToAdjMatrix roadMap
    ```

    2. **Memoization Table Initialization:**
    - A memoization table (`dp`) is initialized to store the minimal cost and the next city to visit for each state. Each state is represented by a pair `(currentCity, visitedSet)`.
    - The `visitedSet` is represented by a bitmask, where each bit corresponds to a `cityIndex` (1 as visited, 0 as unvisited), e.g. `0011` means cities 0 and 1 visited.
    - Each entry stores a `Maybe (Distance, Int)` tuple: `Distance` is the minimal cost to complete the tour from the current state, and `Int` is the next city to visit

    ```haskell
    dp :: Data.Array.Array (Int, Int) (Maybe (Distance, Int))
    dp = Data.Array.listArray ((0, 0), (n-1, (1 `Data.Bits.shiftL` n) - 1)) (repeat Nothing)
    ```

    3. **Recursive TSP Function (`tsp`):**
    - The `tsp` function takes the current city, the set of visited cities, and the memoization table as inputs.
    - **Base Case:** If all cities have been visited, it returns the distance from the current city back to the starting city.
    - **Recursive Case:** For each unvisited city, it calculates the total cost of visiting that city and recursively solves the subproblem. It updates the memoization table with the minimal cost and the next city to visit.

    [Deep Explanation of TSP Recursive Case](#deep-explanation-of-tsp-recursive-case)

    ```haskell
    tsp :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Int)) -> (Distance, Data.Array.Array (Int, Int) (Maybe (Distance, Int)))
    tsp currentCity visited dpMemo
        | visited == (1 `Data.Bits.shiftL` n) - 1 =
            case m Data.Array.! (currentCity, 0) of
                Just d  -> (d, dpMemo)
                Nothing -> (inf, dpMemo)
        | otherwise =
            case dpMemo Data.Array.! (currentCity, visited) of
                Just (cost, _) -> (cost, dpMemo)
                Nothing ->
                    let
                        (minCost, dpUpdated) = foldl nextCity (inf, dpMemo) [0..n-1]
                        nextCity (currentMin, dpAcc) nextCityIdx
                            | visited Data.Bits..&. (1 `Data.Bits.shiftL` nextCityIdx) /= 0 = (currentMin, dpAcc)
                            | otherwise =
                                case m Data.Array.! (currentCity, nextCityIdx) of
                                    Just d ->
                                        let
                                            visited' = visited Data.Bits..|. (1 `Data.Bits.shiftL` nextCityIdx)
                                            (costNext, dpNext) = tsp nextCityIdx visited' dpAcc
                                            totalCost = d + costNext
                                            (newMin, dpNew) = if totalCost < currentMin
                                                then (totalCost, dpNext Data.Array.// [((currentCity, visited), Just (totalCost, nextCityIdx))])
                                                else (currentMin, dpNext)
                                        in (newMin, dpNew)
                                    Nothing -> (currentMin, dpAcc)
                    in (minCost, dpUpdated)
    ```

    4. **Reconstructing the Optimal Path:**
    - After computing the minimal cost, the `reconstructPath` function is used to reconstruct the optimal path by following the next city pointers from the memoization table.

    [Deep Explanation of Recontruct Path Recursive Case](#deep-explanation-of-reconstruct-path-recursive-case)

    ```haskell
    reconstructPath :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Int)) -> [City]
    reconstructPath currentCity visited dpMemo
        | visited == (1 `Data.Bits.shiftL` n) - 1 =
            [indexToCity cityIndices currentCity, indexToCity cityIndices 0]
        | otherwise =
            case dpMemo Data.Array.! (currentCity, visited) of
                Just (_, nextCityIdx) ->
                    indexToCity cityIndices currentCity : reconstructPath nextCityIdx (visited Data.Bits..|. (1 `Data.Bits.shiftL` nextCityIdx)) dpMemo
                Nothing -> []
    ```

    5. **Execution:**
    - The function initializes the visited set with the starting city and executes the `tsp` function starting from city 0.
    - If the minimal cost is infinite, it returns an empty list, indicating no valid path exists. Otherwise, it reconstructs and returns the optimal path.

    ```haskell
    initialVisited = 1 `Data.Bits.shiftL` 0
    (minCost, dpFinal) = tsp 0 initialVisited dp
    if minCost >= inf then []
    else
        let path = reconstructPath 0 initialVisited dpFinal
        in path
    ```

    [Look for Time and Space Complexity of the Algorithm](#complexity)

    ---

    ### Deep Explanation of TSP Recursive Case
    [Back to the DP Algorithm Explanation](#dp-algorithm-explanation)

    The algorithm explores all possible next cities that have not yet been visited, calculates the total cost of visiting each city, and recursively solves the subproblem for each possible next city.

    1. **Identify Unvisited Cities**:
    - It iterates over all possible next cities (`nextCityIdx`) and checks if each city has already been visited using a bitmask (`visited`).

    2. **Skip Visited Cities**:
    - If a city has already been visited (i.e., its bit is set in the `visited` bitmask), the algorithm skips it.

    ```hs
    | visited Data.Bits..&. (1 `Data.Bits.shiftL` nextCityIdx) /= 0 = (currentMin, dpAcc)
    ````

    3. **Calculate Total Cost**:
    - For each unvisited city, the algorithm retrieves the distance from the current city to the next city from the adjacency matrix (`m`).
    - And marks the next city as visited by updating the `visited` bitmask.

    ```hs
    case m Data.Array.! (currentCity, nextCityIdx) of
    Just d -> -- Distance currentCity -> nextCity

    visited' = visited Data.Bits..|. (1 `Data.Bits.shiftL` nextCityIdx)
    ```

    4. **Recursively Solve the Subproblem**:
    - It recursively calls the `tsp` with the next city as the current city and the updated `visited` bitmask.

    ```hs
    (costNext, dpNext) = tsp nextCityIdx visited' dpAcc
    ```

    5. **Total Cost**:
    - The total cost of visiting the next city is the sum of distance to next city and the minimum cost to complete the tour from the next city.
    - If total cost of visiting the next city is less than current minimal cost, it updates the minimal cost and the memoization table.

    ```hs
    totalCost = d + costNext

    (newMin, dpNew) = if totalCost < currentMin
    then (totalCost, dpNext Data.Array.// [((currentCity, visited), Just (totalCost, nextCityIdx))])
    else (currentMin, dpNext)
    ```

    ---

    ### Deep Explanation of Reconstruct Path Recursive Case
    [Back to the DP Algorithm Explanation](#dp-algorithm-explanation)
    
    The reconstructPath function reconstructs the path of the TSP from the memoization table `dpMemo`.

    1. **Function Signature**:
    - `currentCity`: The current city index.
    - `visited`: The bitmask representing the set of visited cities.
    - `dpMemo`: The memoization table storing the minimal cost and next city for each state.
    - Returns a list of cities representing the path.
    
    ```hs
    reconstructPath :: Int -> Int -> Data.Array.Array (Int, Int) (Maybe (Distance, Int)) -> [City]
    ```

    2. **Base Case**:
    - If all cities have been `visited` (all bits as `1`), the function returns the current city and the starting city (city 0) to complete the tour.
    - It uses the auxiliar function `indexToCity` that converts the cityIndex into the City.

    ```hs
    | visited == (1 `Data.Bits.shiftL` n) - 1 =
    [indexToCity cityIndices currentCity, indexToCity cityIndices 0]
    ```

    3. **Recursive Case**:
    - If not all cities have been visited, the function looks up the next city to visit from the `dpMemo` table.
    - `Just (_, nextCityIdx)`: Retrieves the next city index from the memoization table.
    The function then recursively calls itself with the next city and the updated `visited` bitmask.
    - `indexToCity cityIndices currentCity`: Converts the current city index to the actual city and adds it to the path.
    - `visited Data.Bits..|. (1 Data.Bits.shiftL nextCityIdx)`: Updates the visited bitmask to include the next city.
    > If the memoization table entry is `Nothing`, it returns an empty list, indicating an error in the path reconstruction.

    ### Complexity
    **- ⏱️ Time Complexity:** O(n<sup>2</sup>2<sup>n</sup>)`*`
    
    **- 🚀 Space Complexity:** O(n 2<sup>n</sup>)

    `*` where O(n 2<sup>n</sup>) are maximum number of unique subproblems/states and O(n) for transition (through for loop as in code) in every states.





    

    

