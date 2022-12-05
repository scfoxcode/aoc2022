#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
// clang++ main.cpp -o ans -std=c++17 && ./ans
//
typedef std::vector<char> Stack;
typedef std::vector<Stack> Stacks;

struct Move {
    int count;
    int from; // index from 0
    int to; // index from 0
};

std::vector<std::string> readFile(std::ifstream& file) {
    std::vector<std::string> contents;
    std::string line;
    while (std::getline(file, line)) {
        contents.push_back(line);
    }
    return contents;
}

Stacks buildInitialStacks(std::vector<std::string>& contents) {
    Stacks stacks;
    for (int i=0; i<9; i++) {
        stacks.push_back(Stack());
    }
    for (int i=7; i>-1; i--) {
        for (int j=0; j<9; j++) {
            char value = contents[i][j*4 + 1];
            if (value != ' ') {
                stacks[j].push_back(value);
            }
        }
    }
    return stacks;
}

Move moveFromString(std::string line) {
    std::vector<std::string> words;
    std::stringstream input(line);
    std::string part;
    while (input >> part) {
        words.push_back(part); 
    }
    // -1 on from and to so we index from 0
    return {
        .count = atoi(words[1].c_str()),
        .from = atoi(words[3].c_str()) -1,
        .to = atoi(words[5].c_str()) -1
    };
}

std::vector<Move> readMoves(std::vector<std::string>& contents) {
    std::vector<Move> moves;
    for (int i=contents.size() - 1; i>9; i--) {
        moves.push_back(moveFromString(contents[i]));
    }
    return moves;
}

void applyMovePart1(Move& move, Stacks& stacks) {
    for (int i=0; i<move.count; i++) {
        stacks[move.to].push_back(stacks[move.from].back());
        stacks[move.from].pop_back();
    }
}

void applyMovePart2(Move& move, Stacks& stacks) {
    Stack temp;
    for (int i=0; i<move.count; i++) {
        temp.push_back(stacks[move.from].back());
        stacks[move.from].pop_back();
    }
    while (temp.size() > 0) {
         stacks[move.to].push_back(temp.back());
         temp.pop_back();
    }
}

void withFile(void (*partFunc)(std::ifstream&)) {
    std::ifstream file;
    file.open("input.txt");
    partFunc(file);
    file.close();
}

void part(std::ifstream& file, int index, void (*applyFunc)(Move&, Stacks&)) {
    std::vector<std::string> contents = readFile(file);
    std::vector<Move> moves = readMoves(contents);
    Stacks stacks = buildInitialStacks(contents);
    while (moves.size() > 0) {
        applyFunc(moves.back(), stacks);
        moves.pop_back();
    }
    std::string answer = "";
    for (int i=0; i<stacks.size(); i++) {
        answer += stacks[i].back();
    }
    printf("Part %i answer %s End ans\n", index, answer.c_str());
}

void part1(std::ifstream& file) {
    part(file, 1, applyMovePart1);
}

void part2(std::ifstream& file) {
    part(file, 2, applyMovePart2);
}

int main() {
    withFile(part1);
    withFile(part2);
    printf("Problem 5\n");
}
