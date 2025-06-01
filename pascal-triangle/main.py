"""

Run the doctests with:

echo ./main.py | entr -c python3 -m doctest /_

Run the program with:

python3 ./main.py

"""


def gen_row(row):
    """
    NOTE: zip is basically almost like Haskell's "transpose"

    >>> gen_row([1])
    [1, 1]

    >>> gen_row(gen_row([1]))
    [1, 2, 1]

    >>> gen_row(gen_row(gen_row([1])))
    [1, 3, 3, 1]

    >>> gen_row(gen_row(gen_row(gen_row([1]))))
    [1, 4, 6, 4, 1]

    """
    return [1] + [sum(pair) for pair in zip(row, row[1:])] + [1]


def pascal(n):
    """
    >>> pascal(5)
    [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1]]
    """
    rows = [[1]]
    for i in range(n-1):
        rows.append(gen_row(rows[-1]))
    return rows


if __name__ == "__main__":
    for row in pascal(5):
        print(row)
