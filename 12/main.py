import sys
import time

BIGSCORE = 1000000000

class Node: 
    def __init__(self, x, y, elevation, isEnd):
        self.x = x
        self.y = y
        self.elevation = elevation 
        self.isEnd = isEnd 
        self.noPath = False
        self.smallestScore = BIGSCORE
        self.connections = []

    def scoreNodes(self, dest, visited, cap):
        if len(visited) > cap:
            return # early out if we already have a shorter path for part2
        if self.noPath == True:
            return # early out if we've already established no path to finish from here
        if len(visited) < self.smallestScore:
            self.smallestScore = len(visited) 

        else:
            return # early out, no point exploring this branch

        # try sorting by manhatten distance to go down the more likely routes first
        # for part 1 this actually makes it slightly slower, but in the general case I know
        # it is likely much faster. Setting reverse to True slowed it down by 4*
        dist = lambda a: abs(a.x - dest.x) + abs(a.y - dest.y)
        sortedC = sorted(self.connections, key=dist, reverse=False)

        if len(self.connections) <1: # flag so we don't search on other paths
            if not self.isEnd:
                self.noPath = True
                print('HAPPENED')
                # go backwards through visited and mark them all false if only 1 connection
                for node in reversed(visited):
                    if len(node.connections) < 2:
                        print('BLAH')
                        node.noPath = True
                    else:
                        break

        for node in sortedC:
            if node not in visited:
                newlist = visited.copy()
                newlist.append(self)
                node.scoreNodes(dest, newlist, cap)

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
                #start[0] == x and start[1] == y,
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

def shortestPath(start, end, nodes, cap=BIGSCORE):
    start.scoreNodes(end, [], cap)
    print('Finding path...')
    return end.smallestScore

def resetScores(nodes):
    for n in nodes:
        n.smallestScore = BIGSCORE

def buildGroupsOfLetter(nodes, letter):
    groups = []
    letterVal = ord(letter)
    print('Building groups of letter {}'.format(letter))
    building = False
    group = None
    # only horizontal, ignoring vertical for now to keep code simpler
    for n in nodes:
        if n.elevation != letterVal:
            if building:
                building = False
                groups.append(group)
            continue
        if not building:
            building = True
            group = []
        group.append(n)
    return groups

def part2():
    # Can we take advantage of the fact that many 'a' items are grouped together
    # Pick from one in the group, should be able to rule out many with this
    # Additionally, we can rule out a group early if any item in that group
    # can't reach the destination
    # we still need to make the single case faster first
    print('Part 2 Idea')

def main():
    print('AOC day 12')
    sys.setrecursionlimit(10000)
    print('Recursion limit {}'.format(sys.getrecursionlimit()))
    testgrid = loadGrid(loadLines('input2.txt'))
    grid = loadGrid(loadLines('input.txt'))
    shortest = shortestPath(testgrid[0], testgrid[1], testgrid[2])
    startTime = time.time()
    shortestPart1 = shortestPath(grid[0], grid[1], grid[2])
    endTime = time.time()
    print('Shortest Test = {}'.format(shortest))
    print('Shortest Part 1 = {} Time Taken = {}'.format(shortestPart1, endTime-startTime))
    #31 is correct on the test
    #456 is correct 
    print('Part 2. Start from all "a" elevations')
    starts = list(filter(lambda n : n.elevation == ord('a'), grid[2]))
    print('Number of a elevations {}'.format(len(grid[2])))
    #return # skip for now
    part2Score = BIGSCORE
    groups = buildGroupsOfLetter(grid[2], 'a')
    print('Number of horizontal groups {}'.format(len(groups)))
    groupResults = []
    smallestSoFar = BIGSCORE
    resetScores(grid[2])
    for i, group in enumerate(groups):
        numIgnored = 0
        for j in grid[2]:
            if j.noPath:
                numIgnored += 1
        print('Checking group - {} num ignored {}'.format(i, numIgnored))
        # look at first element of group to build a group score
        # also record the size of the group
        #resetScores(grid[2]) # commenting this out is dodgy
        #incredibly lucky that I didn't need to look in individual groups
        dist = shortestPath(group[0], grid[1], grid[2], smallestSoFar)
        if dist < smallestSoFar:
            smallestSoFar = dist # so for future searches we can early out sooner
        groupResults.append((dist, len(group), i))
    print (groupResults)
    #print('Part 2 answer = {}'.format(part2Score))

main()
