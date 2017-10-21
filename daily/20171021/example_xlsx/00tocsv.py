import xlrd
import csv


def csv_from_excel(filename, outfile):

    wb = xlrd.open_workbook(filename)
    sh = wb.sheet_by_index(0)

    with open(outfile, "w") as your_csv_file:
        wr = csv.writer(your_csv_file, quoting=csv.QUOTE_ALL)

        for rownum in range(sh.nrows):
            wr.writerow(sh.row_values(rownum))


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    parser.add_argument("dst")
    args = parser.parse_args()
    csv_from_excel(args.src, args.dst)
