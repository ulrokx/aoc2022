if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    tots = []
    tot = 0
    for line in lines:
        if line == "":
            tots.append(tot)
            tot = 0
        else:
            tot += int(line)
    print(sum(sorted(tots, reverse=True)[:3]))
