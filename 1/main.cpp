#include <iostream>
#include <fstream>
#include <cstdlib>

int main() {
    std::ifstream file;
    file.open("input.txt");
    std::string line;
    int food = 0;
    int first, second, third = 0;

    while (std::getline(file, line)) {
        if (line.size() == 0) {
            if (food > first) {
                third = second;
                second = first;
                first = food;
            } else if (food > second) {
                third = second;
                second = food;
            } else if (food > third) {
                third = food;
            }
            food = 0;
        } else {
            food += atoi(line.c_str());
        }
    }

    printf("Max food %i\n", first + second + third);

    file.close();
    return 0;
}
