#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <algorithm>
#include <iterator>
#include <string>
#include <utility>
#include <ctype.h>
#include <chrono>

using namespace std;

#define dbcout if (! debugging) {} else cerr
bool debugging = false;

#define TIMEIT(e) \
  { \
    auto start = std::chrono::high_resolution_clock::now(); \
    e; \
    auto end = std::chrono::high_resolution_clock::now();\
    std::chrono::duration<double> elapsed = end-start; \
    cout << "TIME: " << elapsed.count() << "s\n";  \
  }

enum Dir { UP, RIGHT, DOWN, LEFT };
Dir operator++(Dir& d)
{
    d = static_cast<Dir>((d + 1) % 4);
    return d;
}
Dir operator--(Dir& d)
{
    d = static_cast<Dir>((d +3) % 4);
    return d;
}

enum State { Clean, Weakened, Infected, Flagged };
State operator++(State& st)
{
  st = static_cast<State>((st + 1) % 4);
  return st;
}
State operator--(State& st)
{
  st = static_cast<State>((st +3) % 4);
  return st;
}

typedef map<pair<int,int>, State> Grid;

void turnBySt(const State& st, Dir& d)
{
  switch (st) {
  case Clean: --d; return;
  case Weakened: return;
  case Infected: ++d; return;
  case Flagged: ++d; ++d; return;
  }
}

void step(int& x, int& y, const Dir& d)
{
  switch (d) {
  case UP: y++; break;
  case LEFT: x--; break;
  case DOWN: y--; break;
  case RIGHT: x++; break;
  }
}

int solve1 (Grid GRID, int N)
{
  int X = 0;
  int Y = 0;
  Dir d = UP;
  int infect = 0;

  while (N--) {
    auto xy = make_pair(X,Y);
    const State oldSt = GRID[xy];
    if (oldSt == Clean) {
      GRID[xy] = Infected;
      infect++;
    } else if (oldSt == Infected) {
      GRID.erase(xy);
    }
    turnBySt(oldSt, d);
    step(X,Y,d);
  }
  return infect;
}

int solve2 (Grid GRID, int N)
{
  int X = 0;
  int Y = 0;
  Dir d = UP;
  int infect = 0;

  while (N--) {
    auto xy = make_pair(X,Y);
    const State oldSt = GRID[xy];

    switch (oldSt) {
    case Clean:
      GRID[xy] = Weakened;
      break;
    case Flagged:
      GRID.erase(xy);
      break;
    case Weakened:
      infect++; // fallthrough
    default:
      ++GRID[xy];
      break;
    }

    turnBySt(oldSt, d);
    step(X,Y,d);
  }
  return infect;
}

int main (int argc, char *argv[])
{
  Grid GRID;
  {
    string file(argc > 1 ? argv[1] : "input.txt");
    ifstream input(file);

    vector<string> lines;
    std::copy(std::istream_iterator<string>(input),
              std::istream_iterator<string>(),
              std::back_inserter(lines));

    int y = (lines.size() -1) /2;
    for (auto l : lines) {
      int x = -1 * ((l.size() -1)/2);
      for (auto c : l) {
        if (c == '#') {
          GRID.insert(make_pair(make_pair(x,y), Infected));
        }
        x++;
      }
      y--;
    }
  }

  TIMEIT(cout << solve1(GRID, 10000) << endl);
  TIMEIT(cout << solve2(GRID, 10000000) << endl);
}
