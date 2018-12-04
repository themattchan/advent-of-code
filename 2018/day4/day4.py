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
                evt = m.group(6)[0]
                newGuard = m.group(7)
                x = int(newGuard) if newGuard != None else evt
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

# given a set of ranges, for each min in the hour, count how many times that minute occs in the set
# build_freqs_map :: list range[datetime] -> map int int
def build_freqs_map(rs):
  rs = list(map(lambda d: (d[0].minute, d[1].minute),rs))
  def gen():
    for i in range(0,59):
      x = ft.reduce((lambda acc,r: acc + inrange(i,r)), rs, 0)
      yield (i, x)
  return dict(gen())

# build a map of all guards and their sleeping intervals
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

events = parse('input.txt')

sleepmap = build_sleepmap(events)
sleepyGuard = max(sleepmap.keys(), key=lambda k: ft.reduce(lambda acc, r: acc+rangesize(r), sleepmap.get(k), dt.timedelta()))

freqs = build_freqs_map(sleepmap[sleepyGuard])

bestTime = best_minute(freqs)[0]

## part 1
print (bestTime * sleepyGuard)



# all_guards_best = { k: best_minute(build_freqs_map(v)) for k, v in sleepmap.items() }
# maxSleepyGuard = max(all_guards_best.keys(), key=lambda k: all_guards_best.get(k)[1])
# print (maxSleepyGuard * all_guards_best[maxSleepyGuard][0])
