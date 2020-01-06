"""
This example demonstrates the use of the SQLAlchemy job store.
On each run, it adds a new alarm that fires after ten seconds.
You can exit the program, restart it and observe that any previous alarms that have not fired yet
are still active. You can also give it the database URL as an argument.
See the SQLAlchemy documentation on how to construct those.
"""

from datetime import datetime, timedelta
import logging
import os
import pathlib

from apscheduler.schedulers.blocking import BlockingScheduler
from handofcats import as_command


def alarm(time):
    print("Alarm! This alarm was scheduled at %s." % time)


@as_command
def run():
    logging.basicConfig(level=logging.DEBUG)

    scheduler = BlockingScheduler()
    scheduler.add_jobstore("memory")

    alarm_time = datetime.now() + timedelta(seconds=1)
    scheduler.add_job(alarm, "date", run_date=alarm_time, args=[datetime.now()])
    alarm_time = datetime.now() + timedelta(seconds=1)
    scheduler.add_job(alarm, "date", run_date=alarm_time, args=[datetime.now()])
    alarm_time = datetime.now() + timedelta(seconds=2)
    scheduler.add_job(alarm, "date", run_date=alarm_time, args=[datetime.now()])

    print("To clear the alarms, delete the example.sqlite file.")
    print("Press Ctrl+{0} to exit".format("Break" if os.name == "nt" else "C"))

    try:
        scheduler.start()
    except (KeyboardInterrupt, SystemExit):
        pass
