try:
    import lib2to3
except ImportError:
    import blib2to3 as lib2to3

print(f"** {lib2to3.__name__} **")

def test_it():
    2 == 2
