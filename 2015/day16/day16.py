import re

def parse(file):
  def go():
    with open(file) as input:
      for l in input:
        m = re.match('Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)', l)
        n = int(m.group(1))
        x = [ (m.group(2), int(m.group(3))), (m.group(4), int(m.group(5))), (m.group(6), int(m.group(7))) ]
        yield n, x
  return go()

eq_attrs = { 'children': 3, 'samoyeds': 2, 'akitas': 0, 'vizslas': 0, 'cars': 2, 'perfumes': 1 }
gt_attrs = { 'cats': 7, 'trees': 3 }
lt_attrs = { 'pomeranians': 3, 'goldfish': 5 }
all_attrs = dict(gt_attrs, **lt_attrs, **eq_attrs)

def solve(entries, eqs, lts={}, gts={}):
  def f(x):
    i, xs = x
    return (   all([eqs[k] == v for k,v in xs if k in eqs])
           and all([lts[k] > v  for k,v in xs if k in lts])
           and all([gts[k] < v  for k,v in xs if k in gts])
           )
  return list(filter(f, entries))[0][0]

print(solve(parse('input'), all_attrs))
print(solve(parse('input'), eq_attrs, lts=lt_attrs, gts=gt_attrs))
