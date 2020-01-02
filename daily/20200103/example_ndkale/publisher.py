from handofcats import as_command
from taskworker import tasks


@as_command
def run(*, n: int) -> None:
    # n 'The input of fibonacci task. Default: 10'
    tasks.FibonacciTask.publish(None, n)
    print(f"A FibonacciTask is scheduled to run. With input {n}.")
