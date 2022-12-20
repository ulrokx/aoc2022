def solve(lines: list[str]) -> str:
    towers = [[] for _ in range(9)]
    for col in range(9):
        for row in range(7, -1, -1):
            box = lines[row][4*col + 1]
            if box == ' ':
                break
            towers[col].append(box)

    instructions = lines[10:]
    for i in instructions:
        words = i.split(" ")
        quantity = int(words[1])
        frum = int(words[3])
        to = int(words[5])
        for _ in range(quantity):
            towers[to - 1].append(towers[frum - 1].pop())
    return "".join(list(map(lambda t: t[-1], towers)))


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())
    print(solve(lines))
