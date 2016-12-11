from pathlib import Path

# pathlib does not append trailing slashes, but jsonref needs that.
url = Path('/absolute/path/to/base/dir').as_uri() + '/'
print(url)
