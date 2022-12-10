from copy import deepcopy
knots = []

tailLocations = {
    "0_0": True
}

def posForDiagonal(posA, posB):
    x = (posB[0] + 1) if (posA[0] > posB[0]) else (posB[0] - 1);
    y = (posB[1] + 1) if (posA[1] > posB[1]) else (posB[1] - 1);
    return (x, y) 

def posForLinear(posA, posB):
    if posA[0] > posB[0]:
        return (posB[0] + 1, posB[1])
    if posA[0] < posB[0]:
        return (posB[0] - 1, posB[1])
    if posA[1] > posB[1]:
        return (posB[0], posB[1] + 1)
    if posA[1] < posB[1]:
        return (posB[0], posB[1] - 1)


def isAdjacent(posA, posB):
    diffX = abs(posA[0] - posB[0])
    diffY = abs(posA[1] - posB[1])
    return (diffX < 2 and diffY < 2)

def requiresDiagonal(posA, posB):
    if isAdjacent(posA, posB):
        return False
    diffX = posA[0] != posB[0]
    diffY = posA[1] != posB[1]
    return (diffX and diffY)

def updateKnots(newNext, oldNext, index):
    global knots 
    global tailLocations
    isTail = index == (len(knots) - 1)

    # Special case first move always happens
    if index == 0:
        knots[0] = newNext
        updateKnots(newNext, oldNext, index + 1)
        return

    curPos = deepcopy(knots[index])

    # Early out if we aren't moving, child nodes aren't either
    if isAdjacent(newNext, curPos):
        return 
    else:
        newPos = (0, 0) 
        if requiresDiagonal(newNext, curPos):
            newPos = posForDiagonal(newNext, curPos)
        else:
            newPos = posForLinear(newNext, curPos)

        knots[index] = newPos
        
        if isTail:
            tailLocations[str(newPos[0]) + "_" + str(newPos[1])] = True
        else:
            updateKnots(deepcopy(newPos), deepcopy(curPos), index + 1)

def moveHead(direction):
    vec = (0, 0)
    if direction == "U":
        vec = (0, 1)
    if direction == "D":
        vec = (0, -1)
    if direction == "L":
        vec = (-1, 0)
    if direction == "R":
        vec = (1, 0)

    global knots 
    newPos = (knots[0][0] + vec[0], knots[0][1] + vec[1])
    updateKnots(newPos, deepcopy(knots[0]), 0)

def applyInstruction(line):
    parts = line.split()
    direction = parts[0]
    steps = int(parts[1])

    for i in range(steps):
        moveHead(direction)

def drawTails():
    for y in range(-15, 15):
        for x in range(-15, 15):
            key = str(x) + "_" + str(y * -1)
            if key in tailLocations:
                print("#", end="")
            else:
                print(".", end="")
        print("")

def runTest(name, filepath, numknots, expected=0):
    global knots
    global tailLocations

    with open(filepath) as f:
        lines = f.readlines()
        knots = [(0, 0)] * numknots 
        tailLocations = {
            "0_0": True
        }
        for line in lines:
            applyInstruction(line)
        ans = len(tailLocations)
        f.close()
        if expected != 0:
            msg = 'Pass' if ans == expected else 'FAIL'
            print('{} - {}, ans: {}, expected {}'.format(msg, name, ans, expected))
        else:
            print('{} - {}'.format(name, ans))

def main():
    runTest('Part 1', 'input.txt', 2, 6314)
    runTest('Test 2', 'input2.txt', 10, 36)
    runTest('Part 2', 'input.txt', 10)
    #drawTails()

main()
