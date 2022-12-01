import os

def main():

    elves = []

    # This is an ugly solution, its been a long day
    with open('./test.txt') as f:
        split = f.read().split(2*os.linesep)

        for set in split:
            values = set.split(os.linesep)
            total = sum([int(value) for value in values])

            elves.append(total)

    print(max(elves))

if __name__ == "__main__":
    main()