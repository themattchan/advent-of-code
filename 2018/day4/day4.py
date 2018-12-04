import datetime as dt
import re
import itertools as it
import functools as ft
import operator

def fst(t): return t[0]
def snd(t): return t[1]
def compose(g,f): return lambda x: g(f(x))

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
  return (x <= i and y > i)

def rangesize(r):
  (x,y)=r
  return y-x

def count_in_ranges(rs, i):
  return ft.reduce((lambda acc,r: acc + inrange(i,r)), rs, 0)

def sum_ranges(rs, zero):
  return ft.reduce(lambda acc, r: acc+rangesize(r), rs, zero)

def dt_ranges_to_min_ranges(rs):
  return list(map(lambda d: (d[0].minute, d[1].minute),rs))

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

def max_on_dict(getter,m):
  k = max(m.keys(), key=compose(getter,m.get))
  return (k, m[k])

def max_dict(m):
  return max_on_dict(lambda x: x,m)

def fmap_dict(f, m):
  return { k: f(v) for k, v in m.items() }

################################################################################

def part1(sleepmap):
  sleepy_guard = fst(max_on_dict(lambda rs: sum_ranges(rs, dt.timedelta()), sleepmap))
  # print ('sleepy_guard='+str(sleepy_guard))
  freqs = build_freqs_map(sleepmap[sleepy_guard])
  best_time = fst(max_dict(freqs))

  return (best_time * sleepy_guard)

def part2(sleepmap):
  def go():
    for i in range(0,60):
      # for each guard, compute how many times he is asleep in minute i,
      # and find the guard who sleeps the most in minute i
      x = max_dict(fmap_dict(lambda rs: count_in_ranges(dt_ranges_to_min_ranges(rs), i), sleepmap))
      yield (i,x)

  # for all minutes, guard who sleeps the most.
  best_guards_map = dict(go())

  best_min, (best_guard,_) = max_on_dict(snd, best_guards_map)

  return (best_min * best_guard)

sleepmap = build_sleepmap(parse('input.txt'))
print('part1 = '+str(part1(sleepmap)))
print('part2 = '+str(part2(sleepmap)))