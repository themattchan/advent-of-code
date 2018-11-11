import re
import itertools as it

def parse(f):
    G = {}
    with open(f) as input:
        for l in input:
            m = re.match("(\w+) to (\w+) = ([0-9]+)", l)
            g1 = G.get(m.group(1), {})
            g1[m.group(2)] = int(m.group(3))
            G[m.group(1)]=g1
            g2 = G.get(m.group(2),{})
            g2[m.group(1)] = int(m.group(3))
            G[m.group(2)]=g2
    return G

# brute force undirected hamiltonian circuit
def solve(G):
#    print G
    def solve1(ps):
        return sum(it.imap(lambda (x,y): G[x][y], it.izip(ps, it.islice(it.cycle(ps), 1, len(ps)))))
    return it.imap(solve1, it.permutations(G.keys()))


#    print G
if __name__ == "__main__":
    x = parse("input")
    print min(solve(x))
    print max(solve(x))
