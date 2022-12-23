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
        self.sensorHash = set() 
        self.beaconHash = set()

    def addSensor(self, sensor):
        self.sensors.append(sensor)
        self.sensorHash.add(sensor.hash())

    def addBeacon(self, beacon):
        self.beacons.append(beacon)
        self.beaconHash.add(beacon.hash())

    def cantBeBeaconCount(self, row):
        # All positions, sorted by x position
        notAllowed = set() 
        print('PLEASE', len(notAllowed) )

        # See which sensors cover the position
        # use spation partitioning so we don't need to check them all
        for sensor in self.sensors:
            yDiff = abs(sensor.y - row)
            xRadius = sensor.closestDistance - yDiff
            if xRadius < 0:
                continue # This sensor does not overlap with any positions
            xStart = sensor.x - xRadius 
            xEnd = sensor.x + xRadius
            for x in range(xStart, xEnd + 1):
                pos = Position(x, row) 
                hash = pos.hash()
                if hash in self.beaconHash:
                    continue # If we know this is a beacon, early out
                if hash not in notAllowed:
                    notAllowed.add(hash)

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
        # print ('sensor {} {} beacon {} {}'.format(sensor.x, sensor.y, beacon.x, beacon.y))
        board.addBeacon(beacon)
        board.addSensor(sensor)

    return board

def part1():
    board = readBoard('input.txt')
    print('Num beacons' , len(board.beacons))
    cannotBeCount = board.cantBeBeaconCount(2000000) 
    # Correct ans is 5870800
    print('Part 1 ans', cannotBeCount)

def part2():
    print('Part 2 ans', 0)

def main():
    print('Aoc day 15')
    part1()
    part2()

main()
