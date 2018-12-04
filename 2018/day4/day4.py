import datetime as dt
import re
import itertools as it
import functools as ft
import operator

def fst(t): return t[0]

# type event = 'w' | 'f' | guard-number

# parse :: file -> list (date, event)
def parse(file):
    def go(file):
        with open(file) as input:
            for l in input:
                m = re.match('\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (wakes up|falls asleep|Guard #(\d+) begins shift)', l)
                d = dt.datetime(int(m.group(1)), int(m.group(2)), int(m.group(3)), int(m.group(4)), int(m.group(5)))
                event = m.group(6)[0]
                new_guard = m.group(7)
                x = int(new_guard) if new_guard != None else event
                yield (d,x)
    return sorted(go(file), key=fst)

# splitdt :: datetime -> (date,time)
def splitdt(d):
  return (dt.date(d.year,d.month,d.day), dt.time(d.hour,d.minute,d.second,d.microsecond,d.tzinfo))

# type range = (int,int)

# inrange :: int -> range -> bool
def inrange(i,r):
  (x,y) = r
  return (x <= i and y >= i)

def rangesize(r):
  (x,y)=r
  return y-x

def count_in_ranges(rs, i):
  return ft.reduce((lambda acc,r: acc + inrange(i,r)), rs, 0)

def dt_ranges_to_min_ranges(rs): return list(map(lambda d: (d[0].minute, d[1].minute),rs))

# given a set of ranges, for each min in the hour, count how many times that minute occs in the set
# build_freqs_map :: list range[datetime] -> map int int
def build_freqs_map(rs):
  rs = dt_ranges_to_min_ranges(rs)
  def gen():
    for i in range(0,60):
      yield (i, count_in_ranges(rs,i))
  return dict(gen())

# build a map of all guards and their sleeping intervals
# build_sleepmap :: events -> map guard (list range)
def build_sleepmap(events):
  def go(l, r):
    (acc, cg) = l
    ((d1,e1),(d2,e2)) = r

    # update current guard
    cg = e1 if isinstance(e1,int) else cg
    acc[cg] = acc.get(cg, [])+([(d1,d2)] if (e1 == 'f') else [])
    return (acc,cg)

  sleepmap,_ = ft.reduce(go, zip(events, events[1:]), ({}, None))
  return sleepmap

# best_minute :: freqs_map -> (int, int)
def best_minute(freqs):
  k = max(freqs.keys(), key=freqs.get)
  return (k, freqs[k])

################################################################################

events = parse('input.txt') # parse('test')

sleepmap = build_sleepmap(events)
sleepyGuard = max(sleepmap.keys(), key=lambda k: ft.reduce(lambda acc, r: acc+rangesize(r), sleepmap.get(k), dt.timedelta()))
print ('sleepyGuard='+str(sleepyGuard))
freqs = build_freqs_map(sleepmap[sleepyGuard])

bestTime = best_minute(freqs)[0]

## part 1
print ('part 1='+str(bestTime * sleepyGuard))


def mapValues(f, m):
  return { k: f(v) for k, v in m.items() }

def part2(sleepmap):
    def go():
      for i in range(0,60):
        # for each guard, compute how many times he is asleep in minute i
        x = mapValues(lambda rs: count_in_ranges(dt_ranges_to_min_ranges(rs), i), sleepmap)

        # find the guard who sleeps the most in minute i
        y = max(x.keys(), key=x.get)

        # return him and the number of times he slept
        ret = (i, (y, x[y]))
        print(ret)

        yield ret

    # for all minutes, guard who sleeps the most.
    best_guards_map = dict(go())

    best_min = max(best_guards_map.keys(), key=lambda k: best_guards_map[k][1])
    print(best_min)

    best_guard = best_guards_map[best_min][0]
    print(best_guard)

    return (best_min * best_guard)

events = parse('input.txt') # parse('test')

sleepmap = build_sleepmap(events)

print(part2(sleepmap))