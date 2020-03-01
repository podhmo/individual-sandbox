from handofcats import as_command
from tinylog import print
import use
import multiprocessing

@as_command
def run():
    print("hello")
    use.foo()
    print("byebye")
