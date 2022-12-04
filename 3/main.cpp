#include <iostream>
#include <string>
#include <fstream>
#include <vector>

// each rucksack has 2 large compartments
// a given type cannot be in both compartments
// rucksack always has same number of items in each of it's comparments
// 1 line of input per rucksack, half the chars for each compartment
// assign priority a-z = 1-26 A-Z = 27-52
// only 1 item is in both in each rucksack

std::vector<std::string> splitStringInHalf(std::string s) {
    int size = s.size();
    int half = size / 2;
    std::string compA = s.substr(0, half);
    std::string compB = s.substr(half, half);
    return {compA, compB};
}

char findDuplicate(std::vector<std::string> data) {
    int half = data[0].size();
    int index = 0;

    while (index < half) {
        bool inAll = true;
        for (int i=1; i<data.size(); i++) {
            auto pos = data[i].find(data[0][index]);
            if (pos == std::string::npos) {
                inAll = false;
            }
        }
        if (inAll) {
            return data[0][index];
        }
        index++;
    }
    printf("Error, failed to find duplicate\n");
    return '1';
}

int charToPriority(char item) {
    if (item >= 'a' && item <= 'z') {
        return (item - 'a' + 1);
    } else {
        return (item - 'A' + 27);
    }
}

void part1() {
    std::ifstream file;
    file.open("input.txt");
    std::string line;
    int sum = 0;
    while (std::getline(file, line)) {
        sum += charToPriority(findDuplicate(splitStringInHalf(line)));
    }
    printf("Sum of priorities is %i\n", sum);
}

void part2() {
    std::ifstream file;
    file.open("input.txt");
    std::string line1;
    std::string line2;
    std::string line3;
    int sum = 0;
    while (std::getline(file, line1)) {
        std::getline(file, line2);
        std::getline(file, line3);
        sum += charToPriority(findDuplicate({line1, line2, line3}));
    }
    printf("Sum of badge values %i\n", sum);

}

int main() {
    part1();
    part2();
    return 0;
}
