import sys

class Node: 
    def __init__(self, x, y, elevation, isStart, isEnd):
        self.x = x
        self.y = y
        self.elevation = elevation 
        self.isStart = isStart 
        self.isEnd = isEnd 
        self.smallestScore = 100000000
        self.connections = []

    def scoreNodes(self, visited=[]):
        if len(visited) < self.smallestScore:
            self.smallestScore = len(visited) 
        else:
            return # early out, no point exploring this branch

        #exists = list(filter(lambda n: n.x == node.x and n.y == node.y))
        for node in self.connections:
            if node not in visited:
                newlist = visited.copy()
                newlist.append(self)
                node.scoreNodes(newlist)

def nodeFromXY(gridWidth, gridHeight, x, y, allNodes):
    index = gridWidth * y + x
    if x >=0 and y >= 0 and x < gridWidth and y < gridHeight:
        return allNodes[index]
    else:
        return None

def buildConnections(gridWidth, gridHeight, node, allNodes):
    connections = []
    adjacent = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    for offset in adjacent:
        x2 = node.x + offset[0]
        y2 = node.y + offset[1]
        neighbour = nodeFromXY(gridWidth, gridHeight, x2, y2, allNodes)
        if neighbour != None:
            isLegal = (neighbour.elevation - node.elevation) < 2 
            if isLegal:
                connections.append(neighbour)
    return connections

def loadGrid(lines):
    start = None 
    end = None 
    for y, row in enumerate(lines):
        if start != None and end != None:
            break
        if 'S' in row:
            start = (row.find('S'), y)
        if 'E' in row:
            end = (row.find('E'), y)
    # replace start and end letters with elevation to simplify code
    lines[start[1]] = lines[start[1]].replace('S', 'a') 
    lines[end[1]] = lines[end[1]].replace('E','z')

    # lets return a list of nodes instead
    nodes = []
    gridWidth = len(lines[0])
    gridHeight = len(lines)
    for y, row in enumerate(lines):
        for x, val in enumerate(row): 
            node = Node(x, y,
                ord(lines[y][x]),
                start[0] == x and start[1] == y,
                end[0] == x and end[1] == y)
            nodes.append(node)

    for node in nodes:
        node.connections = buildConnections(gridWidth, gridHeight, node, nodes)

    return ( # return start node, end node and full nodes list
        nodes[gridWidth * start[1] + start[0]],
        nodes[gridWidth * end[1] + end[0]],
        nodes)

def loadLines(filepath):
    with open(filepath) as f:
        lines = f.read().splitlines()
    return lines 

def shortestPath(start, end, nodes):
    start.scoreNodes()
    print('Finding path...')
    return end.smallestScore

def main():
    print('AOC day 12')
    sys.setrecursionlimit(10000)
    print('Recursion limit {}'.format(sys.getrecursionlimit()))
    testgrid = loadGrid(loadLines('input2.txt'))
    grid = loadGrid(loadLines('input.txt'))
    shortest = shortestPath(testgrid[0], testgrid[1], testgrid[2])
    shortestPart1 = shortestPath(grid[0], grid[1], grid[2])
    print('Shortest Test = {}'.format(shortest))
    print('Shortest Part 1= {}'.format(shortestPart1))
    #31 is correct on the test
    #457 is too high

main()
