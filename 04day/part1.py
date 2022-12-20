def solve(line: str) -> int:
    first, second = line.split(",")
    f1, f2 = first.split("-")
    s1, s2 = second.split("-")
    f1, f2, s1, s2 = int(f1), int(f2), int(s1), int(s2)
    if (f1 <= s1 and f2 >= s2) or (s1 <= f1 and s2 >= f2):
        return 1
    return 0


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line)
    print(sum(map(solve, lines)))
