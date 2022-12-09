class Action:
    def __init__(self, op="nop", value=None, value2=None):
        self.op = op
        self.value = value
        self.value2 = value2 # what a hack value2 lol

class Node:
    def __init__(self, name, size = 0):
        self.name = name
        self.size = 0
        self.parent = None

class File(Node):
    def __init__(self, name, size = 0):
        super().__init__(name, size)
        # what is the point of super if it wasnt working for size
        self.name = name
        self.size = size
        self.type = 'file'

    def calcSize(self):
        return self.size

class Directory(Node):
    def __init__(self, name, size = 0):
        super().__init__(name, size)
        self.type = 'directory'
        self.name = name
        self.size = size
        self.children = []
    
    def hasChild(self, node):
        match = list(filter(
            lambda n:
                n.name == node.name and 
                n.type == node.type
            , self.children))
        return len(match) > 0

    def addChild(self, node):
        if not self.hasChild(node):
            node.parent = self
            self.children.append(node)

    def calcSize(self):
        size = 0
        for child in self.children:
            size += child.calcSize()
        return size

    def goToRoot(self):
        while self.parent:
            return self.parent.goToRoot()
        return self

    def goUpOne(self):
        if self.parent:
            return self.parent
        else:
            return self

    def goDownOne(self, name):
        match = list(filter(
            lambda n:
                n.name == name and 
                n.type == 'directory' 
            , self.children))
        if len(match) > 0:
            return match[0]
        else:
            return self

def actOnEveryDirectory(node, action):
    if node.type == 'directory':
        action(node)
        for child in node.children:
            actOnEveryDirectory(child, action)

class FileSystem:
    def __init__(self, node):
        self.pointer = node

    def applyAction(self, action):
        if action.op == 'cd':
            if action.value == '/':
                self.pointer = self.pointer.goToRoot()
            elif action.value == '..':
                self.pointer = self.pointer.goUpOne()
            else:
                self.pointer = self.pointer.goDownOne(action.value)
        elif action.op == 'mkfile':
            self.pointer.addChild(File(action.value, int(action.value2)))
        elif action.op == 'mkdir':
            self.pointer.addChild(Directory(action.value))

    # get a flat list of all directories in the tree
    def getSizes(self):
        fs.pointer = fs.pointer.goToRoot() # Starting from root
        root = fs.pointer
        sizes = []
        actOnEveryDirectory(
            root,
            lambda node: sizes.append(node.calcSize()))
        return sizes

def parseLine(line):
    parts = line.split() 
    if parts[0] == '$':
        if parts[1] == 'cd':
            return Action('cd', parts[2])
        else: # only ls, which is a nop for our tree
            return Action()
    elif parts[0] == 'dir':
        return Action('mkdir', parts[1])
    else: # only the <size filename> case remains
        return Action('mkfile', parts[1], parts[0]) 

def part1(sizes):
    total = 0;
    for size in sizes:
        if size <= 100000:
            total += size
    print ('Total size: ', total)

def part2(sizes):
    total = 70000000 
    need = 30000000
    used = sizes[0] # should be the first size in the list
    print ('Used space', used)
    tofree = need - (total - used)
    print ('Need to free', tofree)
    closest = total 
    for size in sizes:
        if size >= tofree and size < closest:
            closest = size
    print ('Closest size to free', closest)


with open ('input.txt') as f:
    lines = f.readlines()
    fs = FileSystem(Directory('/'))
    actions = list(map (parseLine, lines)) # use reduce
    list(map (fs.applyAction, actions)) # build our file system
    sizes = fs.getSizes();
    part1(sizes)
    print ('')
    part2(sizes)

