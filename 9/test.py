import main

def testRequireDiagonals():
    print('Test: requireDiagonal')
    tests = [
        (main.requiresDiagonal((0,0), (0,0)), False),
        (main.requiresDiagonal((1,0), (0,0)), False),
        (main.requiresDiagonal((0,0), (1,1)), False),
        (main.requiresDiagonal((-1,-1), (0,0)), False),
        (main.requiresDiagonal((-2,0), (0,0)), False),
        (main.requiresDiagonal((1,0), (1,0)), False),
        (main.requiresDiagonal((-1,0), (-1,0)), False),
        (main.requiresDiagonal((-2,0), (-0,1)), True),
        (main.requiresDiagonal((2,2), (0,0)), True),
        (main.requiresDiagonal((1,0), (2,4)), True),
        (main.requiresDiagonal((-2,-2), (0,0)), True)
    ] 
    for i, test in enumerate(tests):
        msg = 'Pass' if test[0] == test[1] else 'FAIL'
        print('{} - {}'.format(msg, i))

def testIsAdjacent():
    print('Test: isAdjacent')
    tests = [
        (main.isAdjacent((2,0), (0,0)), False),
        (main.isAdjacent((-2,0), (0,0)), False),
        (main.isAdjacent((0,0), (2,0)), False),
        (main.isAdjacent((0,0), (-2,0)), False),
        (main.isAdjacent((5,0), (-2,0)), False),
        (main.isAdjacent((0,0), (0,0)), True),
        (main.isAdjacent((1,1), (0,0)), True),
        (main.isAdjacent((4,4), (4,4)), True),
        (main.isAdjacent((-1,0), (-2,0)), True),
        (main.isAdjacent((3,-4), (2,-4)), True),
    ] 
    for i, test in enumerate(tests):
        msg = 'Pass' if test[0] == test[1] else 'FAIL'
        print('{} - {}'.format(msg, i))

def testPosForDiagonal():
    print('Test: posForDiagonal')
    tests = [
        (main.posForDiagonal((2,2), (0,0)), (1,1)),
        (main.posForDiagonal((2,3), (0,1)), (1,2)),
        (main.posForDiagonal((-4,5), (-2,4)), (-3,5)),
        (main.posForDiagonal((6,15), (7,17)), (6,16)),
        #(main.posForDiagonal((0,0), (0,0)), (0,0)),
    ] 
    for i, test in enumerate(tests):
        msg = 'Pass' if test[0] == test[1] else 'FAIL'
        print('{} - {}'.format(msg, i))


def runTests():
    print('Running Tests')
    testRequireDiagonals()
    testIsAdjacent()
    testPosForDiagonal()

runTests()
