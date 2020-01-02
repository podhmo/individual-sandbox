from kale import worker
from handofcats import as_command


@as_command
def run() -> None:
    print("Task worker is running ...")
    worker.Worker().run()
