FROM python:3.8-slim

COPY requirements.lock ./
RUN python3 -m pip --no-cache-dir install -r requirements.lock
CMD ["python3"]
