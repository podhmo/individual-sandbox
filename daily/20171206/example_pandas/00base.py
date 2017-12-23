import numpy as np
import pandas as pd


def merge0(series, start_time, end_time):
    summary_series = series.groupby(level=0).sum()
    merged = pd.Series([], index=[])
    indices = summary_series.index
    for d in pd.date_range(start_time, end_time):
        if d in indices:
            merged = merged.append(pd.Series([summary_series[d]], index=[d]))
        else:
            merged = merged.append(pd.Series([0], index=[d]))
    return merged.dropna().sort_index(ascending=True)


def merge1(series, start_time, end_time):
    summary_series = series.groupby(level=0).sum().sort_index(ascending=True)
    indices = pd.date_range(start_time, end_time)
    base_series = pd.Series(np.zeros(len(indices)), index=indices)
    merged = base_series + (summary_series[indices[0]:indices[-1]])
    return merged.fillna(0).astype("int64")


ranges = pd.date_range("2017-11-20", "2018-01-10")
series = pd.Series(np.arange(len(ranges)), index=ranges)
series = series.drop(ranges[::3])
series = series.append(series)

from pandas.util.testing import assert_series_equal  # noqa
assert_series_equal(
    merge0(series, "2017-12-01", "2017-12-31"), merge1(series, "2017-12-01", "2017-12-31")
)
