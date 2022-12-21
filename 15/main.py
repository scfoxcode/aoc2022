import re

class Position:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    @staticmethod
    def dist(a, b):
        return int(abs(a.x - b.x) + abs(a.y - b.y))

    def hash(self):
        return '{}_{}'.format(self.x, self.y)

class Beacon(Position):
    pass

class Sensor(Position):
    def __init__(self, x, y, beacon):
        super().__init__(x, y)
        self.closestBeacon = beacon 
        self.closestDistance = Position.dist(self, self.closestBeacon) 

class Board():
    def __init__(self):
        self.sensors = []
        self.beacons = []
        self.gridHash = {}

    def addSensor(self, sensor):
        self.sensors.append(sensor)
        self.gridHash[sensor.hash()] = True

    def addBeacon(self, beacon):
        self.beacons.append(beacon)
        self.gridHash[beacon.hash()] = True

    def cantBeBeaconCount(self, row):
        xMin =  100000000
        xMax = -100000000
        manhatMax = 0

        # Find the max number of positions in the row we would need to test
        for s in self.sensors:
            if s.x < xMin:
                xMin = s.x
            if s.x > xMax:
                xMax = s.x
            if s.closestDistance > manhatMax:
                manhatMax = s.closestDistance

        xMin -= manhatMax
        xMax += manhatMax

        # All positions, sorted by x position
        #positions = list(map(lambda x: Position(x, row), range(xMin, xMax + 1)))
        notAllowed = {}

        # See which sensors cover the position
        # use spation partitioning so we don't need to check them all
        for sensor in self.sensors:
            yDiff = abs(sensor.y - row)
            xRadius = sensor.closestDistance - yDiff
            if xRadius <= 0:
                continue # This sensor does not overlap with any positions
            xStart = sensor.x - xRadius 
            xEnd = sensor.x + xRadius
            for x in range(xStart, xEnd + 1):
                pos = Position(x, row) 
                hash = pos.hash()
                if hash not in notAllowed:
                    notAllowed[hash] = True

        print('Beacons')
        return len(notAllowed) 

def readBoard(filepath):
    board = Board()
    with open(filepath) as f:
        lines = f.read().splitlines()
        f.close()

    for line in lines:
        coords = re.findall(r'x=(-?\d+), y=(-?\d+)', line)
        beacon = Beacon(int(coords[1][0]), int(coords[1][1]))
        sensor = Sensor(int(coords[0][0]), int(coords[0][1]), beacon)
        board.addBeacon(beacon)
        board.addSensor(sensor)

    return board

def part1():
    board = readBoard('input.txt')
    cannotBeCount = board.cantBeBeaconCount(2000000) 
    # 5870798 is too low... really?
    print('Part 1 ans', cannotBeCount)

def part2():
    print('Part 2 ans', 0)

def main():
    print('Aoc day 15')
    part1()
    part2()

main()
