-- This is a lua implementation of the algorithm described in
-- http://devmag.org.za/2009/05/03/poisson-disk-sampling/
--
-- The structure of the algorithm is exactly the same than in 
-- the mentioned article. Its pseudo-code snippets were translated
-- to Lua.
--
-- One detail worths an explanation, though. The article uses a 2D matrix
-- called grid to store coordinates of points. In the article, it is
-- assumed that grid elements can be accesed via grid[point], being point
-- some structure with a pair of x and y integers, so grid[point] should
-- be equivalent to grid[x,y] or grid[x][y]. This grid is assumed to be
-- initially dimensioned and filled by nils.
--
-- In my implementation the grid is dynamic, and it is an associative array
-- indexed by string keys in the form grid["(x,y)"]. The function gridToString()
-- can be used to convert a Point to its string form, so the grid is indeed
-- accesed like this: grid[gridToString(p)] being p a Point with integer
-- coordinates (which in fact is found via imageToGrid, like in the article)

-- UTILITY FUNCTIONS (used in the article, without giving implementation)
-- =====================================================================

-- RandomQueue stores values and gives them in random order

RandomQueue = {}
function RandomQueue.new ()
    return {last=-1}
end

function RandomQueue.push(q, item) 
    local last = q.last + 1
    q.last = last
    q[last] = item
end

function RandomQueue.pop(q)
    if (RandomQueue.empty(q)) then 
        return nil
    end
    local index = math.random(0,q.last) 
    -- A random index is generated. The last element
    -- is swaped with this random item, and the new
    -- last item is popped.
    local last = q.last
    item = q[index]
    q[index] = q[last]
    q[last] = nil
    q.last = last -1
    return item
end

function RandomQueue.empty(q)
    return q.last==-1
end

function RandomQueue.print(q)
    -- For debugging. Not used
    local t = {}
    for i=0, q.last do
        table.insert(t, string.format("(%f,%f)", q[i].x, q[i].y))
    end
    print (string.format("RandomQueue %d elem: %s", q.last+1, table.concat(t, " ")))
end

-- Point stores a coordinate pair
Point = {}

function Point.new(x,y)
    return {x=x, y=y}
end

-- Determines if a point is inside the rendered rectangle
function inRectangle(point, width, height)
  return (point.x>0 and point.y>0 and point.x<width and point.y<height)
end

-- Converts a point to a string representation, to be used as index in the grid
function gridToString(gridPoint)
    return string.format("(%d,%d)", gridPoint.x, gridPoint.y)
end

-- Computes the distance between two points
function distance(p1, p2)
    return math.sqrt(math.pow(p2.x-p1.x,2) + math.pow(p2.y-p1.y,2))
end

-- Prints the grid. For debugging. Not used
function printGrid(grid)
    print "==========="
    for k,v in pairs(grid) do
        print (string.format("%s:  %f, %f", k, v.x, v.y))
    end
end

-- THE FUNCTIONS GIVEN IN THE ARTICLE
-- This is the lua implementation of the pseudocode in the article


function generate_poisson(width, height, min_dist, new_points_count)
    local cellSize = min_dist/math.sqrt(2)
    local grid = {} -- Point.new(math.ceil(width/cellSize), math.ceil(height/cellSize))}
    local processList = RandomQueue.new()
    local samplePoints = {};  -- Empty list

    -- Generate the first point
    local firstPoint = Point.new(math.random()*width, math.random()*height)
    -- print (string.format("newPoint: [%f, %f]", firstPoint.x, firstPoint.y))
    RandomQueue.push(processList, firstPoint)
    table.insert(samplePoints, firstPoint)
    grid[gridToString(imageToGrid(firstPoint, cellSize))] = firstPoint

    -- Generate other points from points in queue
    while (not RandomQueue.empty(processList)) do
        -- RandomQueue.print(processList)
        -- printGrid(grid)
        local point = RandomQueue.pop(processList)
        for i=0,new_points_count do
            local newPoint = generateRandomPointAround(point, min_dist)
            -- print (string.format("newPoint: [%f, %f]", newPoint.x, newPoint.y))
            -- Check the point is in the region and not too close
            -- to other points
            if inRectangle(newPoint, width, height) and 
                not inNeighbourhood(grid, newPoint, min_dist, cellSize) then
                -- In this case, the point is accepted
                RandomQueue.push(processList, newPoint)
                table.insert(samplePoints, newPoint)
                grid[gridToString(imageToGrid(newPoint, cellSize))] = newPoint;
            end
        end
    end
    return samplePoints
end

function imageToGrid(point, cellSize)
    local gridX = math.floor(point.x/cellSize)
    local gridY = math.floor(point.y/cellSize)
    return Point.new(gridX, gridY)
end

function generateRandomPointAround(point, mindist)
    local r1 = math.random()
    local r2 = math.random()
    local radius  = mindist * (r1+1)
    local angle   = 2 * math.pi * r2
    newX = point.x + radius * math.cos(angle)
    newY = point.y + radius * math.sin(angle)
    return Point.new(newX, newY)
end

function inNeighbourhood(grid, point, mindist, cellSize)
    local gridPoint = imageToGrid(point, cellSize)
    cellsAroundPoint = squareAroundPoint(grid, gridPoint, 5)
    for k,cell in pairs(cellsAroundPoint) do
        if not (cell==nil) then
            local d = distance(cell, point)
            if distance(cell, point) < mindist then
                return true
            end
        end
    end
    return false
end

-- This one is not given in the article. It returns the
-- values of several cells around the give gridPoint
-- We are using string indexes for the grid, but if we
-- try to access to a key which is not stored, lua gives
-- nil instead of an exception, so it works as expected
-- because we get nils for cells which have no dot inside
function squareAroundPoint(grid, gridPoint, n)
    local extreme = math.floor(n/2)
    local result = {}
    for i=-extreme,extreme do
        for j=-extreme,extreme do
            ii = i + gridPoint.x
            jj = j + gridPoint.y
            data = grid[gridToString(Point.new(ii,jj))]
            if data == nil then
                repr = "nil"
            else
                repr = string.format("(%f,%f)", data.x, data.y)
            end
            table.insert(result, data)
        end
    end
    return result
end


-- Initialize random seed
math.randomseed(os.time())

-- Function to generate the list of dots in a tikz's foreach compatible syntax
function poisson_points_list(width, height, mindist, add_points)
    local data = generate_poisson(width, height, mindist, add_points)
    local str = {}
    for k,v in pairs(data) do
        table.insert(str, string.format("%f/%f", v.x, v.y))
    end
    tex.print(table.concat(str, ", "))
end
