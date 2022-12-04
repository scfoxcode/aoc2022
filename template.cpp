#include <iostream>
#include <fstream>
#include <string>

void withFile(void (*partFunc)(std::ifstream&)) {
    std::ifstream file;
    file.open("input.txt");
    partFunc(file);
    file.close();
}

void part1(std::ifstream& file) {
}

void part2(std::ifstream& file) {
}

void tests() {
}

int main() {
    withFile(part1);
    withFile(part2);
    tests();
    printf("Problem x\n");
}
