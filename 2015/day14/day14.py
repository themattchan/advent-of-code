import re
import itertools as it

def snd(x): return x[1]

def calc(s,t,r):
    def go(n):
        (x,y) = divmod (n, t+r)
        return s * ((x*t) + min(t,y))
    return go    

def parse(f):
    def go(f):
        with open(f) as input:
            for l in input:
                m = re.match("(\w+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.", l)
                n = m.group(1)
                s = int(m.group(2))
                t = int(m.group(3))
                r = int(m.group(4))
                yield n, calc(s,t,r)
    return dict(go(f))

def solve1(x,n):
    return max([(k, f(n)) for k,f in x.iteritems()], key=snd)

def solve2(x,n):
    score = dict.fromkeys(x,0)
    for i in range(1,n):
        y = solve1(x,i)[1]
        a = [k for k,f in x.iteritems() if f(i) == y]
        for k in a:
            score[k] += 1
    return max(score.iteritems(), key=snd)
    
if __name__ == "__main__":
    T = parse("test")
    print solve1(T, 1000)
    print solve2(T, 1000)

    X = parse("input")
    print solve1(X, 2503)
    print solve2(X, 2503)
