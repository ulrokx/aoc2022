def solve(line: str) -> int:
    first, second = line.split(",")
    f1, f2 = first.split("-")
    s1, s2 = second.split("-")
    f1, f2, s1, s2 = int(f1), int(f2), int(s1), int(s2)
    f, s = (f1, f2), (s1, s2)
    fs = [f, s]
    fs.sort()
    if fs[0][1] >= fs[1][0]:
        return 1
    return 0


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line)
    print(sum(map(solve, lines)))
