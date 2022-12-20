import re

class Position:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    @staticmethod
    def dist(a, b):
        return int(abs(a.x - b.x) + abs(a.y - b.y))

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

    def addSensor(self, sensor):
        self.sensors.append(sensor)

    def addBeacon(self, beacon):
        self.beacons.append(beacon)

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
        print('Beacons')

    def couldPositionContainBeacon(self, position):
        for sensor in self.sensors:
            # check if sensor is in this square
            #if sensor.x == position.x and sensor.y == position.y:
            #    return False

            dist = Position.dist(position, sensor)
            if dist < sensor.closestDistance:
                return False

        return True

    def minX(self):
        smallest = 100000000
        positions = self.sensors + self.beacons
        for p in positions:
            if p.x < smallest:
                smallest = p.x
        return smallest

    def maxX(self):
        biggest = -100000000
        positions = self.sensors + self.beacons
        for p in positions:
            if p.x > biggest:
                biggest = p.x
        return biggest 

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
    minX = board.minX() 
    maxX = board.maxX() + 1 
    cannotBeCount = 0 
    # ungodly slow, but also wrong answer. I don't want to fix first part until I get the second
    for i in range(minX, maxX):
        if not board.couldPositionContainBeacon(Position(i, 2000000)):
            cannotBeCount += 1

    # 5870798 is too low... really?
    print('Part 1 ans', cannotBeCount)

def part2():
    print('Part 2 ans', 0)

def main():
    print('Aoc day 15')
    part1()
    part2()

main()
