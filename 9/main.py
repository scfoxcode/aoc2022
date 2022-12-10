from copy import deepcopy
knots = []

tailLocations = {
    "0_0": True
}

def requiresDiagonal(posA, posB):
    diffX = posA[0] != posB[0]
    diffY = posA[1] != posB[1]
    return diffX and diffY 

def posForDiagonal(posA, posB):
    diffX = posA[0] - posB[0]
    diffY = posA[1] - posB[1]
    x = (posB[0] + 1) if diffX > 0 else (posB[0] - 1);
    y = (posB[1] + 1) if diffY > 0 else (posB[1] - 1);
    return (x, y) 

def isAdjacent(posA, posB):
    diffX = abs(posA[0] - posB[0])
    diffY = abs(posA[1] - posB[1])
    return diffX < 2 and diffY < 2

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
        newPos = deepcopy(oldNext)
        if requiresDiagonal(newNext, curPos):
            newPos = posForDiagonal(newNext, curPos)

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

    # It's not this simple, only update tail if new pos is not adjacent to head
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

with open('input2.txt') as f:
    lines = f.readlines()

    # Part 1 ans 6314
    knots = [(0, 0)] * 2
    for line in lines:
        applyInstruction(line)
    print("Part 1 Num spots: ", len(tailLocations))

    # Part 2 ans 3385 too high
    print("Start Part 2")
    knots = [(0, 0)] * 10 
    tailLocations = {
        "0_0": True
    }
    for line in lines:
        applyInstruction(line)
    print("Part 2 Num spots: ", len(tailLocations))
    drawTails()

    f.close()

