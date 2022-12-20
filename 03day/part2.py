

def solve(lines):
    a, b, c = map(set, lines)
    same, = a & b & c
    if ord('a') <= ord(same) <= ord('z'):
        return (ord(same) - ord('a')) + 1
    else:
        return (ord(same) - ord('A')) + 27


if __name__ == "__main__":
    n = 3
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    by_three = [lines[i:i+n] for i in range(0, len(lines), n)]
    print(sum(map(solve, by_three)))
