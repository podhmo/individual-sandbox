from __future__ import annotations
import typing as t
import logging
import sys
from handofcats import as_command

if t.TYPE_CHECKING:
    from pandas.core.frame import DataFrame
logger = logging.getLogger(__name__)


@as_command
def run(*, dataset: str, outputfile: str, groupby: str, field: str, fns: t.List[str]):
    from vega_datasets import data

    logger.info("load %s", data)
    df: DataFrame = getattr(data, dataset)()
    print(df.describe(), file=sys.stderr)

    logger.info("groupby %s with %s %s", groupby, field, fns)
    grouped_df: DataFrame = df.groupby(by=groupby).agg({field: fns}).reset_index()

    # flat index
    grouped_df.columns = grouped_df.columns.to_flat_index()
    grouped_df.columns = grouped_df.columns.map(
        lambda xs: xs[0] if not xs[-1] else "-".join(xs)
    )

    print(grouped_df, file=sys.stderr)

    logger.info("save to %s", outputfile)
    grouped_df.to_csv(outputfile)
