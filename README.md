# T06_G05
- Bruno Huang     up202207517     50%
- Ricardo Yang    up202208465     50%

## Brief description of the tasks each one performed

We collaborated on the first seven questions, exchanging ideas, and then Bruno tackled the shortest path problem while Ricardo worked on the traveling salesman problem.

## Shortest Path Function

1. Data Structure Selection and Conversion

    **RoadMap to Adjacency List Conversion**

    The given RoadMap format is a list of tuples where each tuple represents a road between two cities and includes the distance. However, for efficient pathfinding, this format is converted into an adjacency list. An adjacency list provides a more direct representation of each city’s neighbors and their respective distances, making it more efficient for accessing neighboring cities during path exploration.

    AdjList = [(City, [(City, Distance)])]
    - It allows direct access to each city’s neighbors, essential for traversing paths.
    - It improves readability and efficiency compared to repeatedly scanning through a RoadMap for neighbors.

    <ins>roadMapToAdjList</ins>:
    
    The ***roadMapToAdjList*** function converts the **RoadMap** to an **AdjList** by iterating over each road in the roadmap and adding the connection between two cities (both directions) to create a bidirectional adjacency list.

    ```bash
    roadMapToAdjList :: RoadMap -> AdjList
    roadMapToAdjList [] = []
    roadMapToAdjList ((c1, c2, dist):rest) = 
        let adjList = roadMapToAdjList rest
        in addEdge c1 c2 dist (addEdge c2 c1 dist adjList)
    ```

    <ins>addEdge</ins>

    The ***addEdge*** function is a helper that adds a direct connection from one city to another with a specified distance. It appends city2 as a neighbor of city1 if city1 is already in the adjacency list. If city1 is not in the list, it creates a new entry with city2 as its neighbor.

    ```bash
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

    ```bash
    isCityInRoadMap :: City -> RoadMap -> Bool
    isCityInRoadMap city roadMap = any (\(c1, c2, _) -> c1 == city || c2 == city) roadMap
    ```

    Then the function transforms the **roadMap** into an **adjList**

    The BFS-based approach processes each city layer by layer to guarantee the shortest paths (in terms of distance) are identified first. Each queue entry (as we couldn't import any other data types than the given ones we used a list to represent the bfs queue) includes the **<ins>current city</ins>**, the **<ins>accumulated path</ins>**, and the **<ins>current total distance</ins>**. This iterative process builds up the path from start to goal as follows:

    ```bash
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

    **Time Complexity:** O(V + ElogE)

    **Space Complexity:** O(V + E)

## Travel Sales Function