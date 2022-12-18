import sys

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
    

def orderCorrect(packet1, packet2, depth=0):
    for i, item1 in enumerate(packet1):
        if i < len(packet2):
            item2 = packet2[i]
        else:
            return False # Right side has ran out of items, return false

        t1 = type(item1)
        t2 = type(item2)
        if t1 != list and t2 != list:
            if item1 < item2:
                return True 
            elif item1 > item2:
                return False
        else:
            list1 = item1 if t1 == list else [item1]
            list2 = item2 if t2 == list else [item2]
            result = orderCorrect(list1, list2, depth+1)
            if result != None:
                return result

    if len(packet1) < len(packet2):
        return True
    else:
        return True if depth == 0 else None

def testOrderCorrect():
    print ('ORDER CORRECT TEST')
    tests = [
        # true tests first
        (([], []), True),
        (([], [1]), True),
        (([1], [1]), True),
        (([1], [2]), True),
        (([], [[]]), True),
        (([], [[[]]]), True),
        (([], [[[1]]]), True),
        (([[2]], [[[3]]]), True),
        (([1, 2], [2, 4]), True),
        (([[4, 4], 4, 4], [[4, 4], 4, 4, 4]), True),
        (([[1], [2, 3, 4]], [[1], 4]), True),
        (([[[0,[5,0,6,8],1,[],1],[8],5,4],[[2,9]],[[9,[10]]],[]], [[10,[[3],[10,3,7,10,7],1,9],[3]]]), True),
        (([[5], [6]], [5, [[[[[[8]]]]]]]), True),
        (([[[],7]], [[4],[[0,3,10,[0]]],[[0]],[7,[7],1,8],[]]), True),

        (([[5], [6]], [5, [[[[[[4]]]]]]]), False),
        (([1], []), False),
        (([2], [1]), False),
        (([[[4]]], [3]), False),
        (([[4]], [[[3]]]), False),
        (([1, 1, 1], [1, 1]), False),
        (([9], [[8, 7, 6]]), False),
        (([7, 7, 7, 7], [7, 7, 7]), False),
        (([[[]]], [[]]), False),
        (([[]], []), False),
        (([1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]), False),

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
        if orderCorrect(packets[i], packets[i+1]) == True:

            correct += pairIndex 
            indices.append(pairIndex)
    print ('Part 1 Answer = {}'.format(correct))
    print (indices)

def main():
    print('Aoc day 13')
    #testPacketFromInput()
    testOrderCorrect()
    part1() # 2804 is wrong :( 

main()
