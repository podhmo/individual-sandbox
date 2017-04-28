import pandas

print(pandas.Timestamp('2010-01-01 00:00:00') + 10000 * pandas.tseries.offsets.DateOffset(milliseconds=1000))
