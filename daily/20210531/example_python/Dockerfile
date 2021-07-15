FROM python:3.8-slim-buster

WORKDIR /app

RUN apt-get update && \
 apt-get -y --no-install-recommends install make jq && \
 ln -fs /usr/share/zoneinfo/Asia/Tokyo /etc/localtime && \
 dpkg-reconfigure --frontend noninteractive tzdata && \
 apt-get clean

COPY requirements.* /app/
RUN pip3 install -U pip && pip3 install -r requirements.txt

# for app
COPY *.py /app/

EXPOSE 8501
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
CMD streamlit run app.py --server.port 8501 --server.headless true
