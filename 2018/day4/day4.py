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

# b2i :: bool -> int
def b2i(b):
  return (1 if b else 0)

# solve1 :: list (date, event) -> int
def solve1(events):
    def go(l, r):
      (acc, cg) = l
      ((d1,e1),(d2,e2)) = r

      # update current guard
      cg = e1 if isinstance(e1,int) else cg
      sleeptime = d2-d1 if (e1 == 'f') else dt.timedelta()
      acc[cg] = acc.get(cg, dt.timedelta())+sleeptime
      return (acc,cg)

    sleepmap,_ = ft.reduce(go, zip(events, events[1:]), ({}, None))
    sleepyGuard = max(sleepmap.keys(), key=sleepmap.get)

    def go2(l,r):
      (acc, cg) = l
      ((d1,e1),(d2,e2)) = r

      # update current guard
      cg = e1 if isinstance(e1,int) else cg
      (date1,time1) = splitdt(d1)
      (date2,time2) = splitdt(d2)
      acc = acc+[(time1.minute, time2.minute)] if cg == sleepyGuard and e1 == 'f' else acc

      return (acc,cg)

    sleepintervals,_ = ft.reduce(go2, zip(events, events[1:]), ([], None))

    # given a set of ranges, for each min in the hour, count how many times that minute occs in the set
    def build_freqs_map(rs):
      for i in range(0,59):
        x = ft.reduce((lambda acc,r: acc + b2i(inrange(i,r))), rs, 0)
        yield (i, x)
    freqs_for_mins = dict(build_freqs_map(sleepintervals))
    bestTime = max(freqs_for_mins.keys(), key=freqs_for_mins.get)
    return (bestTime * sleepyGuard)


f = parse('input.txt')
print(solve1(f))
