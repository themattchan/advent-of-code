// compile:  g++ -o day19 day19.cpp -std=c++14

#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <iterator>
#include <string>
#include <locale>
using namespace std;

#define dbcout if (! debugging) {} else cerr

enum Dir { UP, DOWN, LEFT, RIGHT };

#define notspace(c) (c != ' ')

int main ()
{
  bool debugging = false;
  locale LOCALE; // for isalpha

  ifstream input("input.txt");
  vector<string> MAP;
  string line;
  while (getline(input, line)) {
    MAP.push_back(line);
  }
  int HEIGHT = MAP.size();
  int WIDTH = MAP[0].size();
  dbcout << "read input, size is " << HEIGHT << " x " << WIDTH << endl;

  for (auto s : MAP) {
    dbcout << s << endl;
  }
  // x coordinate (horizontal)
  int x = distance(MAP[0].begin(), find_if_not(MAP[0].begin(), MAP[0].end(), [](char c) {return c == ' ';}));
  dbcout << "beginning is " << x << endl;

  // y coordinate (vertical)
  int y = -1;
  Dir d = DOWN;
  string SEEN;
  int steps = 0;

  while (1) {
    steps++;

    switch(d) {
    case UP: y--; break;
    case DOWN: y++; break;
    case LEFT: x--; break;
    case RIGHT: x++; break;
    }

    if (x < 0 || x > WIDTH || y < 0 || y > HEIGHT) {
      dbcout << "DONE " << "x=" <<x << " y=" <<y<<endl;
      break;
    }
    dbcout << "x=" <<x << " y=" <<y<<endl;

    char c = MAP[y][x];
      dbcout << "c is "  <<c << endl;

    if (isalpha(c, LOCALE)) {
      SEEN += c;

      dbcout << "SEEN "  <<c << endl;

      // hack so it doesn't count the last space after 'Z'
      if (c == 'Z') break;
    }

    else if (c == '+') {
      switch(d) {
      case UP: {
        if (notspace(MAP[y][x-1]))
          d = LEFT;
        else if (notspace(MAP[y][x+1]))
          d = RIGHT;
        break;
      }
      case DOWN: {
        if (notspace(MAP[y][x-1]))
          d = LEFT;
        else if (notspace(MAP[y][x+1]))
          d = RIGHT;
        break;
      }
      case LEFT: {
        if (notspace(MAP[y-1][x]))
          d = UP;
        else if (notspace(MAP[y+1][x]))
          d = DOWN;
        break;
      }
      case RIGHT: {
        if (notspace(MAP[y-1][x]))
          d = UP;
        else if (notspace(MAP[y+1][x]))
          d = DOWN;
        break;
      }
      }
    }
  }

  cout << "part 1 " << SEEN << endl;
  cout << "part 2 " << steps << endl;
  return 0;
}
