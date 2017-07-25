from mypy_extensions import TypedDict


class Movie(TypedDict):
    name: str
    year: int


class BookBasedMovie(Movie):
    based_on: str


movie = {"name": "Blade Runner", "year": 1982}  # type: Movie

name = movie['name']  # Okay; type of name is str
print(name)
year = movie['year']  # Okay; type of year is int
print(year)

print(movie["ame"])  # NG; typo

print("foo" + movie["year"])  # NG; type of year is int (not str)
