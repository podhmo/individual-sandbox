# http://mypy.readthedocs.io/en/latest/kinds_of_types.html#typeddict
from mypy_extensions import TypedDict

Movie = TypedDict("Movie", {"name": str, "year": int})
movie = {"name": "Blade Runner", "year": 1982}  # type: Movie

name = movie['name']  # Okay; type of name is str
print(name)
year = movie['year']  # Okay; type of year is int
print(year)
