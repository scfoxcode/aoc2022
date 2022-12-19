from functools import reduce

class Point:
    def __init__(self, x, y, content='air'):
        self.x = x
        self.y = y
        self.content = content # air, rock, sand

    def hash(self):
        return str(self.x) + '_' + str(self.y)

def nodesFromLine(line):
    rawPoints = list(map(lambda p: p.split(','), line.split(' -> ')))
    return list(map(lambda p: Point(int(p[0]), int(p[1]), 'rock'), rawPoints))

# Returns a list of nodes for each line
def parseInput(lines):
    return list(map(nodesFromLine, lines))

# Generate all points from two nodes, including the nodes themselves
# Only works for straight lines!!!
# Returns the starting node, and all points except the end 
def pointsBetweenNodes(start, end):
    points = []
    dx = end.x - start.x
    dy = end.y - start.y
    key = 'x' if dy == 0 else 'y'
    diff = dx + dy # 0 + d<key> we care about gives us d<key>
    for i in range(0, diff, 1 if diff >= 0 else -1):
        px = start.x if key == 'y' else start.x + i
        py = start.y if key == 'x' else start.y + i
        points.append(Point(px, py, 'rock'))
    return points

# Returns all the points in a series of nodes
def pointsFromNodes(nodes):
    points = []
    for i in range(0, len(nodes) -1):
        points.extend(pointsBetweenNodes(nodes[i], nodes[i+1]))
    points.append(nodes[-1]) # manually add last point to complete path
    return points

def buildPointsFromLines(lines):
    nodes = parseInput(lines)
    points = reduce(lambda a, b: a + b, map(pointsFromNodes, nodes))
    return points

def gridFromPoints(points):
    grid = {}
    for p in points:
        grid[p.hash()] = p
    return grid

def gridLowestRock(grid):
    lowest = 0
    for key in grid:
        node = grid[key]
        if node.content == 'rock' and node.y > lowest:
            lowest = node.y

    return lowest

def simulate(grid, part2):
    print('Running Simulation')
    overflow = False
    sand = None
    makeHash = lambda x, y: '{}_{}'.format(x, y)
    lowestRock = gridLowestRock(grid)
    while not overflow:
        if makeHash(500, 0) in grid and part2:
            overflow = True

        elif sand == None:
            sand = Point(500, 0, 'sand')

        elif not part2 and sand.y > lowestRock:
            overflow = True

        elif part2 and sand.y == lowestRock + 1:
            grid[sand.hash()] = sand
            sand = None

        else:
            below = makeHash(sand.x, sand.y + 1)
            belowLeft = makeHash(sand.x - 1, sand.y + 1)
            belowRight = makeHash(sand.x + 1, sand.y + 1)
            if below not in grid or grid[below] == 'air':
                sand.y += 1
            elif belowLeft not in grid or grid[belowLeft] == 'air':
                sand.x -= 1
                sand.y += 1
            elif belowRight not in grid or grid[belowRight] == 'air':
                sand.x += 1
                sand.y += 1
            else:
                grid[sand.hash()] = sand
                sand = None

def part1(lines):
    points = buildPointsFromLines(lines)
    grid = gridFromPoints(points)
    simulate(grid, False)
    numSand = len(list(filter(lambda key: grid[key].content == 'sand', grid)))
    print ('Part 1', numSand)

def part2(lines):
    points = buildPointsFromLines(lines)
    grid = gridFromPoints(points)
    simulate(grid, True)
    numSand = len(list(filter(lambda key: grid[key].content == 'sand', grid)))
    print ('Part 2', numSand)

def main():
    print('AOC day 14')
    with open('input.txt') as f:
        lines = f.read().splitlines() 
        f.close()

    part1(lines)
    part2(lines)

main()
