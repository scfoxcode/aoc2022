#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>

typedef std::pair<std::string, std::string> RawRange;
typedef std::pair<int, int> Range;
typedef std::pair<Range, Range> ParsedLine;

RawRange splitOnDelim(std::string input, std::string delim) {
    std::size_t pos = input.find(delim);
    if (pos == std::string::npos) {
        printf("Error, invalid input, failed to find '%s'\n", delim.c_str());
    }
    std::string first = input.substr(0, pos);
    std::string second = input.substr(pos+1, input.size());
    return {first, second};
}

Range rangeFromStringRange(std::string stringRange) {
    RawRange raw = splitOnDelim(stringRange, "-");
    return {std::atoi(raw.first.c_str()), std::atoi(raw.second.c_str())};
}

ParsedLine parseLine(std::string line) {
    RawRange raw = splitOnDelim(line, ",");
    Range first = rangeFromStringRange(raw.first);
    Range second = rangeFromStringRange(raw.second);
    return {first, second};
}

bool rangeContainsRange(Range one, Range two) {
    return (one.first <= two.first && one.second >= two.second);
}

bool rangePartiallyOverlaps(Range one, Range two) {
    return (
        one.first <= two.first && one.second >= two.first ||
        one.first >= two.first && one.first <= two.second); 
}

void tests() {
    RawRange test = splitOnDelim("12-761,4-13", ",");
    if (test.first == "12-761") {
        printf("PASS - splitOnDelim first\n");
    } else {
        printf("FAIL - splitOnDelim first %s\n", test.first.c_str());
    }
    if (test.second== "4-13") {
        printf("PASS - splitOnDelim second\n");
    } else {
        printf("FAIL - splitOnDelim second %s\n", test.second.c_str());
    }
}

void part1() {
    std::ifstream input;
    input.open("input.txt");
    std::string line;
    int count = 0;
    while (std::getline(input, line)) {
        ParsedLine range = parseLine(line);
        if (rangeContainsRange(range.first, range.second) ||
            rangeContainsRange(range.second, range.first)) {
            count++;
        }
    }
    printf("Number of overlaps %i\n", count);
    input.close();
}

void part2() {
    std::ifstream input;
    input.open("input.txt");
    std::string line;
    int count = 0;
    while (std::getline(input, line)) {
        ParsedLine range = parseLine(line);
        if (rangePartiallyOverlaps(range.first, range.second) ||
            rangePartiallyOverlaps(range.second, range.first)) {
            count++;
        }
    }
    printf("Number of partial overlaps %i\n", count);
    input.close();
}

int main() {
    tests();
    part1();
    part2();
    printf("Problem 4\n");
}
