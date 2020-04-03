import altair as alt
from vega_datasets import data

cars = data.cars()
chart = alt.Chart(cars[:10]).mark_point().encode(x="Acceleration:Q", y="Horsepower:Q")
# chart.interactive()
print(chart.to_json())
