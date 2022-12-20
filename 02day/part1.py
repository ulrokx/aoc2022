
# 0 rock     A X
# 1 paper    B Y
# 2 scissors C Z

score = {
    0: [1, 0, 2],
    1: [2, 1, 0],
    2: [0, 2, 1]
}

#


def solution(moves):
    tot = 0
    for move in moves:
        print(move)
        opp, me = move.split(" ")
        oppn, men = ord(opp) - ord("A"), ord(me) - ord("X")
        winlose = score[men].index(oppn)
        tot += (winlose * 3) + (men + 1)
    return tot


if __name__ == "__main__":
    lines = []
    with open("input.txt") as f:
        for line in f:
            lines.append(line.strip())

    print(solution(lines))
