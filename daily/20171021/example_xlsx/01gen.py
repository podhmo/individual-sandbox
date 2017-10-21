import xlsxwriter
import numpy.random as r

r.seed(1)

workbook = xlsxwriter.Workbook('test.xlsx')
worksheet = workbook.add_worksheet()

xss = r.randint(1, 100000, size=(65535, 15))
for i, xs in enumerate(xss):
    worksheet.write_row(i, 0, xs)
workbook.close()
