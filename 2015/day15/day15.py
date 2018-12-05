from gekko import GEKKO
import re
from functools import reduce
import operator

def parse(file):
    def go():
        with open(file) as input:
            for l in input:
                m = re.match('(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)', l)
                yield m.group(1), { 'capacity': int(m.group(2)), 'durability': int(m.group(3)), 'flavor': int(m.group(4)), 'texture': int(m.group(5)), 'calories': int(m.group(6)) }
    return dict(go())

def product(xs):
    return reduce(operator.mul, xs, 1)

def head_or_else(d, xs):
    return next(iter(xs), d)

def make_solver():
    solver = GEKKO()
    solver.options.SOLVER = 3
    solver.solver_options = ['linear_solver ma97']
    solver.options.IMODE = 3
    return solver

# adapted from example 9
# https://nbviewer.jupyter.org/url/apmonitor.com/wiki/uploads/Main/gekko.ipybn
# this solver only does minimisation so we dualise the NLP program.
def solve1(ingredients):
#    print(ingredients)
    ks = list(ingredients.keys())
    props = list(filter(lambda p: p != 'calories',ingredients[ks[0]].keys()))

    solver = make_solver()

    quantities = { k: solver.Var(value=1, lb=0, ub=100) for k in ks }

    for k,v in quantities.items():
        solver.Equation(- v < 0)
    solver.Equation(sum([v for k,v in quantities.items()]) - 100 == 0)

    solver.Obj(- (product([sum([quantities[k] * ingredients[k][p] for k in ks]) for p in props])))

    solver.solve(disp=False)

    result = { k: int(round(head_or_else(0,v.value.value),0)) for k,v in quantities.items() }

    # have to compute this shit again
    # because solver.options.objfcnval is totally wrong
    objresult = (product([sum([result[k] * ingredients[k][p] for k in ks]) for p in props]))

    return (result, objresult)

def solve2(ingredients):
#    print(ingredients)
    ks = list(ingredients.keys())
    props = list(ingredients[ks[0]].keys())
    props_no_cals = list(filter(lambda p: p != 'calories',props))

    solver = make_solver()

    quantities = { k: solver.Var(value=1, lb=0, ub=100) for k in ks }

    for k,v in quantities.items():
        solver.Equation(- v < 0)
    solver.Equation(sum([v for k,v in quantities.items()]) - 100 == 0)
    solver.Equation(sum([quantities[k] * ingredients[k]['calories'] for k in ks]) - 500 == 0)

    solver.Obj(- (product([sum([quantities[k] * ingredients[k][p] for k in ks]) for p in props])))

    solver.solve(disp=False)

    result = { k: int(round(head_or_else(0,v.value.value),0)) for k,v in quantities.items() }

    # have to compute this shit again
    # because solver.options.objfcnval is totally wrong
    objresult = (product([sum([result[k] * ingredients[k][p] for k in ks]) for p in props_no_cals]))

    return (result, objresult)

#print(make_solver() != make_solver())
ingredients = parse('input')
#print(solve1(ingredients))
print(solve2(ingredients))
