import re
import itertools as it

def parse(f):
    G = {}
    with open(f) as input:
        for l in input:
            m = re.match("(\w+) would (\w+) ([0-9]+) happiness units by sitting next to (\w+)", l)
            x = m.group(1)
            n = int(m.group(3)) * (-1 if m.group(2) == 'lose' else 1)
            y = m.group(4)

            g1 = G.get(x, {})
            g1[y] = n
            G[x]=g1

    return G

# brute force directed hamiltonian circuit
def solve(ks, lookup):
    def solve1(ps):
        xs = it.izip(ps, it.islice(it.cycle(ps), 1, len(ps)+1))
        return sum(it.imap(lambda (x,y): lookup(x,y)+lookup(y,x), xs))
    return it.imap(solve1, it.permutations(ks))

if __name__ == "__main__":
    T = parse("test")
    print max(solve(T.keys(), lambda x,y: T[x][y])) == 330

    G = parse("input")
    print max(solve(G.keys(), lambda x,y: G[x][y]))

    def part2(x,y):
        if x == 'me' or y == 'me':
            return 0
        else:
            return G[x][y]

    print max(solve(G.keys()+['me'], part2))
    
        
    
