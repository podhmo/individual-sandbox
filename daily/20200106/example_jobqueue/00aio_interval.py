import asyncio
import os
from datetime import datetime

from apscheduler.schedulers.asyncio import AsyncIOScheduler
from handofcats import as_command


def tick(prefix: str):
    print(f"{prefix} The time is: %s" % datetime.now())


@as_command
def run() -> None:
    scheduler = AsyncIOScheduler()
    scheduler.add_job(tick, "interval", seconds=0.5, args=("Tick!",))
    scheduler.add_job(tick, "interval", seconds=1.2, args=("Tick!!!",))

    scheduler.start()
    print("Press Ctrl+{0} to exit".format("Break" if os.name == "nt" else "C"))

    # Execution will block here until Ctrl+C (Ctrl+Break on Windows) is pressed.
    try:
        asyncio.get_event_loop().run_forever()
    except (KeyboardInterrupt, SystemExit):
        pass
