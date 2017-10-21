import csv


def csv_csv(filename, outfile):

    with open(filename) as rf:
        r = csv.reader(rf, quoting=csv.QUOTE_ALL)

        with open(outfile, "w") as your_csv_file:
            wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)

            for row in r:
                wr.writerow(row)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    parser.add_argument("dst")
    args = parser.parse_args()
    csv_csv(args.src, args.dst)
