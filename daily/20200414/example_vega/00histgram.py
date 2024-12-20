import altair as alt
from vega_datasets import data

source = data.iris.url

alt.Chart(source).mark_bar().encode(
    alt.X("IMDB_Rating:Q", bin=True),
    y='count()',
)
