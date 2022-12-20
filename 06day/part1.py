def solve(line):
    for i in range(13, len(line)):
        r = line[i-13:i + 1]
        if len(set(r)) == len(r):
            return i + 1


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    print(solve(lines[0]))
