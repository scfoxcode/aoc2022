#include <iostream>
#include <fstream>

// 0 for lose, 1 for draw, 2 for victory
int roundScore(int victory, char hand) {
    int points = 0;
    if (victory == 1) {
        points = 3;
    } else if (victory == 2) {
        points = 6;
    }
    if (hand == 'A') {
        points += 1;
    } else if (hand == 'B') {
        points += 2;
    } else if (hand =='C') {
        points += 3;
    }
    return points;
}

char choiceNeeded(char aPicked, char requiredOutcome) {
    if (requiredOutcome == 'X') { // loss required
        if (aPicked == 'A') {
            return 'C';
        } else if (aPicked == 'B') {
            return 'A';
        } else {
            return 'B';
        }
    } else if (requiredOutcome == 'Y') { // draw required
        return aPicked;
    } else { // win required
        if (aPicked == 'A') {
            return 'B';
        } else if (aPicked == 'B') {
            return 'C';
        } else {
            return 'A';
        }
    }
}

void part1() {
    std::ifstream file;
    file.open("input.txt");
    int score = 0;
    std::string line;
    while(std::getline(file, line)) {
        const char a = line[0]; // Very fragile I know
        const char b = line[2] - 23; // Convert to ABC format
        // handle draw first
        // ugh the difference is annoying.... Should we convert?
        if (a == b) {
            score += roundScore(1, b);
        } else {
            bool didWeWin = false;
            if (a == 'A' && b == 'B' ||
                a == 'B' && b == 'C' ||
                a == 'C' && b == 'A') {
                didWeWin = true;
            }
            score += roundScore(didWeWin ? 2 : 0, b); 
        }
    }
    printf("Score %i\n", score);
    file.close();
}

void part2() {
    std::ifstream file;
    file.open("input.txt");
    int score = 0;
    std::string line;
    while(std::getline(file, line)) {
        const char a = line[0];
        const char b = line[2];
        const int victory = b == 'X' ? 0 : b == 'Y' ? 1 : 2;
        score += roundScore(victory, choiceNeeded(a, b)); 
    }
    printf("Score part 2 %i\n", score);
    file.close();
}

int main() {
    part1();
    part2();
    return 0;
}
