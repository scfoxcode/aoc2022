import sys
import functools

def packetFromInput(line):
    packet = [] 
    buildingDigit = None
    data = line[1:] # remove initial packet [ border
    pchar = 0
    while pchar < len(data):
        char = data[pchar]
        isDigit = char.isdigit()

        if isDigit:
            if buildingDigit == None:
                buildingDigit = char
            else:
                buildingDigit += char

        if not isDigit and buildingDigit != None:
            packet.append(int(buildingDigit))
            buildingDigit = None

        if char == '[':
            packet.append(packetFromInput(data[pchar:]))
            ptemp = pchar+1
            stack = 1
            while stack > 0:
                char = data[ptemp]
                if char == ']':
                    stack -= 1
                if char == '[':
                    stack +=1
                ptemp += 1
            pchar = ptemp -1

        elif char == ']':
            return packet

        pchar += 1

    return packet
             

def testPacketFromInput():
    print ('PACKET FROM INPUT TEST')
    tests = [
        '[4, 5, 1]',
        '[]',
        '[[]]',
        '[[3], [1, [12, [4, 5]]]]',
        '[1,[2,[3,[4,[5,6,7]]]],8,9]',
        '[[],[[[9],[1,5],5],[[9,0,8,2],4,10,2],2,[[3,1,5,2,8],10]],[],[[10,[2,5],[],[2,10,5,4]],10,3],[[[8,5,4,8,7],6,[1,9,3],0,[4,9,8,6]],3,2]]',

    ]
    ans = [
        [4, 5, 1],
        [],
        [[]],
        [[3], [1, [12, [4, 5]]]],
        [1,[2,[3,[4,[5,6,7]]]],8,9],
        [[],[[[9],[1,5],5],[[9,0,8,2],4,10,2],2,[[3,1,5,2,8],10]],[],[[10,[2,5],[],[2,10,5,4]],10,3],[[[8,5,4,8,7],6,[1,9,3],0,[4,9,8,6]],3,2]],

    ]
    packets = list(map(packetFromInput, tests)) 
    for i, packet in enumerate(packets):
        print ('{} - packet test: {} res: {}'.format('PASS' if packet == ans[i] else 'FAIL', i, packet))

   # compare agains the real input 
    with open('input.txt') as f:
        lines = f.read().splitlines()
        packets = list(map(packetFromInput, filter(lambda l: l != "", lines)))
        f.close()
    modifiedLines = list(map(lambda l: l.replace(',', ', '), filter(lambda l: l != "", lines)))
    for i,p in enumerate(packets):
        if str(p) != modifiedLines[i]:
            print('FAILED MATCHING INPUT')
            return
    print('Lines read perfectly')
    

def orderCorrect(packet1, packet2):
    for i, item1 in enumerate(packet1):
        if i < len(packet2):
            item2 = packet2[i]
        else:
            return -1# Right side has ran out of items, return false

        t1 = type(item1)
        t2 = type(item2)
        if t1 != list and t2 != list:
            if item1 < item2:
                return 1 
            elif item1 > item2:
                return -1 
        else:
            list1 = item1 if t1 == list else [item1]
            list2 = item2 if t2 == list else [item2]
            result = orderCorrect(list1, list2)
            if result != 0:
                return result

    if len(packet1) < len(packet2):
        return 1 
    else:
        return 0
        #return 1 if depth == 0 else None

def testOrderCorrect():
    print ('ORDER CORRECT TEST')
    tests = [
        # true tests first
        (([], []), 0),
        (([], [1]), 1),
        (([1], [1]), 0),
        (([1], [2]), 1),
        (([], [[]]), 1),
        (([], [[[]]]), 1),
        (([], [[[1]]]), 1),
        (([[2]], [[[3]]]), 1),
        (([1, 2], [2, 4]), 1),
        (([[4, 4], 4, 4], [[4, 4], 4, 4, 4]), 1),
        (([[1], [2, 3, 4]], [[1], 4]), 1),
        (([[[0,[5,0,6,8],1,[],1],[8],5,4],[[2,9]],[[9,[10]]],[]], [[10,[[3],[10,3,7,10,7],1,9],[3]]]), 1),
        (([[5], [6]], [5, [[[[[[8]]]]]]]), 1),
        (([[[],7]], [[4],[[0,3,10,[0]]],[[0]],[7,[7],1,8],[]]), 1),

        (([[5], [6]], [5, [[[[[[4]]]]]]]), -1),
        (([1], []), -1),
        (([2], [1]), -1),
        (([[[4]]], [3]), -1),
        (([[4]], [[[3]]]), -1),
        (([1, 1, 1], [1, 1]), -1),
        (([9], [[8, 7, 6]]), -1),
        (([7, 7, 7, 7], [7, 7, 7]), -1),
        (([[[]]], [[]]), -1),
        (([[]], []), -1),
        (([1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]), -1),

    ]
    correct = list(map(lambda p: orderCorrect(p[0][0], p[0][1]), tests)) 
    for i, res in enumerate(correct):
        print ('{} - order test: {} res: {} ans: {}'.format('PASS' if res == tests[i][1] else 'FAIL', i, tests[i][0], res))
        
def part1():
    pairs = []
    correct = 0
    indices = []
    with open('input.txt') as f:
        lines = f.read().splitlines()
        packets = list(map(packetFromInput, filter(lambda l: l != "", lines)))
        f.close()
    pairIndex = 0
    for i in range(0, len(packets), 2):
        pairIndex += 1
        if orderCorrect(packets[i], packets[i+1]) > 0:
            correct += pairIndex 
    print ('Part 1 Answer = {}'.format(correct))

def part2():
    with open('input.txt') as f:
        lines = f.read().splitlines()
        packets = list(map(packetFromInput, filter(lambda l: l != "", lines)))
        f.close()
    packets.append([[2]])
    packets.append([[6]])
    packets.sort(key=functools.cmp_to_key(orderCorrect), reverse=True)
    first = packets.index([[2]]) + 1
    second = packets.index([[6]]) + 1
    print ('Part 2 Answer = {} x {} = {}', first, second, first * second)

def main():
    print('Aoc day 13')
    testPacketFromInput()
    testOrderCorrect()
    part1() # 4894 is right 
    part2()

main()
