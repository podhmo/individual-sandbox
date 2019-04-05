import os.path
import pathlib

print(os.path.dirname(__file__))
print(os.path.abspath(__file__))
print(pathlib.Path(__file__).parent)
print(pathlib.Path(__file__).absolute())
print(pathlib.Path(__file__).absolute().parent / "a.py")
print(pathlib.Path(__file__).absolute().as_uri())
