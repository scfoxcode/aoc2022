class CPU:
    def __init__(self, program, screen = None):
        self.cycles = 0 
        self.x_reg = 1
        self.program = program 
        self.pc = 0
        self.currentOpTime = 0
        self.op = None
        self.screen = screen # Should have made a system class composed of components
    
    def executeInstruction(self):
        raw = self.program[self.pc]
        parts = raw.split()
        if parts[0] == 'addx':
            self.op = lambda : self.addx(int(parts[1]))
            self.currentOpTime = 2
        elif parts[0] == 'noop':
            self.currentOpTime = 1
            self.op = lambda : self.noop()

    def hasFinished(self):
        return self.pc >= len(self.program)
    
    def cycle(self):
        if self.hasFinished():
            return # we don't want to overflow :)

        if self.screen:
            self.screen.draw(self.x_reg) 
        
        if self.op == None:
            self.executeInstruction()

        self.currentOpTime -= 1

        if self.currentOpTime < 1: 
            self.op()
            self.op = None
            self.pc += 1

        self.cycles += 1

    def addx(self, val):
        self.x_reg += val

    def noop(self):
        0

class Screen:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.rows = ['']
        self.currentRow = 0

    def draw(self, val):
        curX = len(self.rows[self.currentRow])
        isVisible = curX >= val - 1 and curX <= val + 1

        self.rows[self.currentRow] += '#' if isVisible else '.'
        if len(self.rows[self.currentRow]) >= self.width:
            print(self.rows[self.currentRow])
            self.currentRow += 1
            self.rows.append('')

def signalStrength(cpu, measurements):
    numCyclesNeeded = measurements[-1]
    sum = 0
    for i in range(numCyclesNeeded):
        if (i + 1) in measurements:
            sum += (i + 1) * cpu.x_reg
        cpu.cycle();
    return sum

def test1():
    with open('input2.txt') as f:
        program = f.readlines()
        ans = signalStrength(
            CPU(program),
            [20, 60, 100, 140, 180, 220])
        msg = 'Pass' if ans == 13140 else 'FAIL'
        print('{} - Test 1, expected {} actual {}'.format(msg, 13140, ans))

def part1():
    with open('input.txt') as f:
        program = f.readlines()
        ans = signalStrength(
            CPU(program),
            [20, 60, 100, 140, 180, 220])
        msg = 'Pass' if ans == 14360 else 'FAIL'
        print('{} - Part 1, expected {} actual {}'.format(msg, 14360, ans))

def part2():
    with open('input.txt') as f:
        program = f.readlines()
        hal9000 = CPU(program, Screen(40, 6))
        for i in range(240):
            hal9000.cycle()

def main():
    print('Solving the chip shortage')
    test1()
    part1()
    part2()

main()
