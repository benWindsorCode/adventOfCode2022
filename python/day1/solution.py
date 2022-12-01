import os

def main():

    elves = []

    # This is an ugly solution
    with open('./input.txt') as f:
        lines = f.read()
        
        split = lines.split(2*os.linesep)
        print(split)

        for set in split:
            values = set.split(os.linesep)
            total = 0
            for value in values:
                total += int(value)

            elves.append(total)

    print(max(elves))

if __name__ == "__main__":
    main()