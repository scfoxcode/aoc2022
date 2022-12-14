class Monkey:
    def __init__(self, items, operation, testval, throwto):
        self.items = items
        self.operation = operation
        self.testval = testval 
        self.throwto = throwto
        self.inspected = 0
    
    def test(self, monkeys):
        if len(self.items) == 0:
            print('ERROR')
            return
        item = self.items[0]
        newworry = self.operation(item) # worry increase as monkey inspects
        #newworry = int(newworry / 3) # reduce worry after no damage

        target = 0 if (newworry % self.testval) == 0 else 1
        # hack for part 2
        mnext = monkeys[self.throwto[target]]
        shouldpass = (mnext.operation(newworry) % mnext.testval) == 0
        temp = (newworry / self.testval)
        doespass = (mnext.operation(temp) % mnext.testval) == 0
        newworry = temp if shouldpass == doespass else newworry

        # end hack for part 2


        monkeys[self.throwto[target]].addItem(newworry)
        self.items.pop(0)
        self.inspected += 1

    def addItem(self, item):
        self.items.append(item)

def parseInput(lines):
    index = 0;
    monkeys = []
    while index < len(lines):
        parts = lines[index].split()
        if len(parts) > 0:
            if parts[0] == 'Monkey':
                # get items
                parts = lines[index + 1].split()
                items = []
                p = 2
                while p < len(parts): 
                    items.append(int(parts[p][0:-1] if ',' in parts[p] else parts[p]))
                    p += 1

                # get op
                parts = lines[index + 2].split()
                val = parts[-1]
                if parts[-2] == '+':
                    op = lambda a, b=val: a + (a if b == 'old' else int(b))
                elif parts[-2] == '*':
                    op = lambda a, b=val: a * (a if b == 'old' else int(b)) 
                else:
                    print('Error parsing op')

                # get divisor
                divisor = int(lines[index + 3].split()[-1])

                # get target monkeys
                targetA = int(lines[index + 4].split()[-1])
                targetB = int(lines[index + 5].split()[-1])

                # finally create the monkey
                monkeys.append(Monkey(items, op, divisor, [targetA, targetB]))
        index += 1
    return monkeys

def tests():
    print('Running tests...')
    with open('input.txt') as f:
        lines = f.readlines()
        monkeys = parseInput(lines)
        if len(monkeys) != 8:
            print('FAIL - should be 8 monkeys, received {}'.format(len(monkeys)))
        f.close()

def part(filepath, maxRounds):
    round = 0 
    with open(filepath) as f:
        lines = f.readlines()
        monkeys = parseInput(lines)
        while round < maxRounds:
            for monkey in monkeys:
                while len(monkey.items) > 0:
                    monkey.test(monkeys)
            round += 1

        scores = list(map(lambda m: m.inspected, monkeys))
        print ('Inspections = {}'.format(scores))
        scores.sort(reverse = True) #270918 is too high
        monkeyBusiness = scores[0] * scores[1]
        print ('Monkey business = {}'.format(monkeyBusiness))
        print ()
        f.close()
        return monkeyBusiness

def main():
    print('Advent of Code 2022 Puzzle 11')
    tests()
    testans = part('input2.txt', 20)
    if testans != 10605:
        print('FAIL - should be 10605, received {}'.format(testans))

    part('input.txt', 20) # 151312
    part('input.txt', 10000)
    print('Finished')

main()
