// compile:  g++ -o day19 day19.cpp -std=c++14

#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <string>
#include <ctype.h>

using namespace std;

#define dbcout if (! debugging) {} else cerr

#define notspace(c) (c != ' ')

enum Dir { UP, DOWN, LEFT, RIGHT };

int main ()
{
  bool debugging = false;

  ifstream input("input.txt");
  vector<string> MAP;
  string line;
  while (getline(input, line)) {
    MAP.push_back(line);
  }
  int HEIGHT = MAP.size();
  int WIDTH = MAP[0].size();

  // x coordinate (horizontal)
  int x = distance(MAP[0].begin(), find_if_not(MAP[0].begin(), MAP[0].end(), [](char c) {return c == ' ';}));

  // y coordinate (vertical)
  int y = 0;
  Dir d = DOWN;
  string seen;
  int steps = 0;

  while (1) {
    if (x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT) {
      break;
    }

    char c = MAP[y][x];

    // scrutinise this square
    if (isalpha(c)) {
      seen += c;
    }
    else if (c == ' ') {
      break;
    }
    else if (c == '+') {
      switch(d) {
      case UP:
      case DOWN: {
        if (notspace(MAP[y][x-1]))
          d = LEFT;
        else if (notspace(MAP[y][x+1]))
          d = RIGHT;
        break;
      }
      case LEFT:
      case RIGHT: {
        if (notspace(MAP[y-1][x]))
          d = UP;
        else if (notspace(MAP[y+1][x]))
          d = DOWN;
        break;
      }
      }
    }

    // finished step
    steps++;

    // take next step
    switch(d) {
    case UP: y--; break;
    case DOWN: y++; break;
    case LEFT: x--; break;
    case RIGHT: x++; break;
    }

  }

  cout << "part 1 " << seen << endl;
  cout << "part 2 " << steps << endl;
  return 0;
}
