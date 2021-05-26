from datetime import datetime

s = "2021-05-25T20:31:24.8848799Z"
print(datetime.strptime(s, "%Y-%m-%dT%H:%M:%S.%f%z"))
