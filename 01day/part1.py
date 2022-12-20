if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    max_so_far = 0
    tot = 0
    for line in lines:
        if line == "":
            max_so_far = max(max_so_far, tot)
            tot = 0
        else:
            tot += int(line)
    print(max_so_far)
