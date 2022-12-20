def solve(line):
    L = len(line)
    first, second = set(line[:L // 2]), set(line[L // 2:])
    similar = first & second
    tot = 0
    for item in similar:
        if ord('a') <= ord(item) <= ord('z'):
            tot += (ord(item) - ord('a')) + 1
        else:
            tot += (ord(item) - ord('A')) + 27
    return tot


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    print(sum(map(solve, lines)))
