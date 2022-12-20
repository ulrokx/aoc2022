from collections import defaultdict


def solve(lines: list[str]):
    hm = defaultdict(int)
    stack = ['']
    for line in lines:
        if line.startswith("$ cd .."):
            stack.pop()
        elif line.startswith("$ cd /"):
            stack = ['']
        elif line.startswith("$ cd"):
            stack.append(stack[-1] + line[5:])
        elif line.startswith("$"):
            pass
        elif line.startswith("dir"):
            pass
        else:
            size, _ = line.split()
            for s in stack:
                hm[s] += int(size)
    part1 = sum(filter(lambda s: s <= 100000, hm.values()))
    left = 70000000 - hm['']
    needed = 30000000 - left
    part2 = list(filter(lambda s: s >= needed, sorted(hm.values())))[0]
    return part1, part2


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    print(solve(lines))
