import sys


def count_parens(filename):
    with open(filename, "r") as f:
        lines = f.readlines()

    count = 0
    total_content = ""
    for line_num, line in enumerate(lines):
        # Strip comments
        if ";" in line:
            line = line.split(";")[0]
        total_content += line

        for char in line:
            if char == "(":
                count += 1
            elif char == ")":
                count -= 1
                if count < 0:
                    print(f"Unmatched ')' at line {line_num + 1}")
                    return

    print(f"Final Count: {count}")
    if count > 0:
        print(f"Missing {count} closing parentheses")
    elif count < 0:
        print(f"Extra {-count} closing parentheses")
    else:
        print("Balanced!")


if __name__ == "__main__":
    count_parens(sys.argv[1])
