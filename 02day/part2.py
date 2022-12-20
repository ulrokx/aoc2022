strategy = [
    [2, 0, 1],
    [0, 1, 2],
    [1, 2, 0]
]


def solution(moves):
    tot = 0
    for move in moves:
        opp, out = move.split(" ")
        oppn, outn = ord(opp) - ord("A"), ord(out) - ord("X")
        tot += (outn * 3) + (strategy[outn][oppn] + 1)
    return tot


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())

    print(solution(lines))
