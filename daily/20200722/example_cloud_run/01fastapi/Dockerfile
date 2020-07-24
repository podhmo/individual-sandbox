FROM python:3.8-slim

ENV APP_HOME /app
WORKDIR $APP_HOME
COPY . ./

RUN pip install gunicorn fastapi uvicorn
CMD exec gunicorn app:app --bind :$PORT --timeout 0 -w 2 -k uvicorn.workers.UvicornWorker
