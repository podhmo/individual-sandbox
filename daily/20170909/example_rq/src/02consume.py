from rq import Queue
from redis import Redis

redis_conn = Redis()
q = Queue(connection=redis_conn)

# Getting the number of jobs in the queue
print(len(q))

# Retrieving jobs
queued_job_ids = q.job_ids  # Gets a list of job IDs from the queue
queued_jobs = q.jobs  # Gets a list of enqueued job instances
job = q.fetch_job('my_id')  # Returns job having ID "my_id"
print("@", job)
