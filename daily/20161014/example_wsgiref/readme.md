# summary

元々はpython3でkqueue,epollを使うようになっていることを知りたかった感じ。

```
go:
	Requests per second:    5196.13 [#/sec] (mean)
uvloop + aiohttp (python3.5):
	Requests per second:    952.67 [#/sec] (mean)
aiohttp (python3.5)
	Requests per second:    660.85 [#/sec] (mean)
wsgiref python3.5
	Requests per second:    1259.98 [#/sec] (mean)
wsgiref python2.7
	Requests per second:    601.48 [#/sec] (mean)
```

# go

```
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        
Server Hostname:        localhost
Server Port:            8000

Document Path:          /
Document Length:        11 bytes

Concurrency Level:      6
Time taken for tests:   0.192 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      128000 bytes
HTML transferred:       11000 bytes
Requests per second:    5196.13 [#/sec] (mean)
Time per request:       1.155 [ms] (mean)
Time per request:       0.192 [ms] (mean, across all concurrent requests)
Transfer rate:          649.52 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.2      0       1
Processing:     0    1   0.5      1       6
Waiting:        0    1   0.4      0       6
Total:          1    1   0.5      1       6
ERROR: The median and mean for the waiting time are more than twice the standard
       deviation apart. These results are NOT reliable.

Percentage of the requests served within a certain time (ms)
  50%      1
  66%      1
  75%      1
  80%      1
  90%      1
  95%      2
  98%      2
  99%      3
 100%      6 (longest request)
```

# uvloop + aiohttp (python3.5)

```
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        Python/3.5
Server Hostname:        localhost
Server Port:            8000

Document Path:          /
Document Length:        11 bytes

Concurrency Level:      6
Time taken for tests:   1.050 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      162000 bytes
HTML transferred:       11000 bytes
Requests per second:    952.67 [#/sec] (mean)
Time per request:       6.298 [ms] (mean)
Time per request:       1.050 [ms] (mean, across all concurrent requests)
Transfer rate:          150.71 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.2      0       2
Processing:     2    6   1.6      6      17
Waiting:        2    5   1.5      5      16
Total:          3    6   1.6      6      18

Percentage of the requests served within a certain time (ms)
  50%      6
  66%      6
  75%      7
  80%      7
  90%      8
  95%      9
  98%     11
  99%     12
 100%     18 (longest request)
```

# aiohttp (python3.5)

```
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        Python/3.5
Server Hostname:        localhost
Server Port:            8000

Document Path:          /
Document Length:        11 bytes

Concurrency Level:      6
Time taken for tests:   1.513 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      162000 bytes
HTML transferred:       11000 bytes
Requests per second:    660.85 [#/sec] (mean)
Time per request:       9.079 [ms] (mean)
Time per request:       1.513 [ms] (mean, across all concurrent requests)
Transfer rate:          104.55 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.1      0       0
Processing:     3    9   1.3      8      15
Waiting:        2    8   1.2      8      14
Total:          3    9   1.3      9      15

Percentage of the requests served within a certain time (ms)
  50%      9
  66%      9
  75%      9
  80%     10
  90%     11
  95%     11
  98%     13
  99%     15
 100%     15 (longest request)
```

# python3.5

```
ab -n 6 -n 1000 http://localhost:8000/
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        WSGIServer/0.2
Server Hostname:        localhost
Server Port:            8000

Document Path:          /
Document Length:        11 bytes

Concurrency Level:      1
Time taken for tests:   0.794 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      166000 bytes
HTML transferred:       11000 bytes
Requests per second:    1259.98 [#/sec] (mean)
Time per request:       0.794 [ms] (mean)
Time per request:       0.794 [ms] (mean, across all concurrent requests)
Transfer rate:          204.25 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.0      0       0
Processing:     1    1   0.1      1       3
Waiting:        0    0   0.1      0       3
Total:          1    1   0.2      1       3

Percentage of the requests served within a certain time (ms)
  50%      1
  66%      1
  75%      1
  80%      1
  90%      1
  95%      1
  98%      1
  99%      1
 100%      3 (longest request)
```

# python2.7

```
ab -n 6 -n 1000 http://localhost:8000/
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking localhost (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        WSGIServer/0.1
Server Hostname:        localhost
Server Port:            8000

Document Path:          /
Document Length:        11 bytes

Concurrency Level:      1
Time taken for tests:   1.663 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      166000 bytes
HTML transferred:       11000 bytes
Requests per second:    601.48 [#/sec] (mean)
Time per request:       1.663 [ms] (mean)
Time per request:       1.663 [ms] (mean, across all concurrent requests)
Transfer rate:          97.51 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.1      0       1
Processing:     1    1   0.5      1       9
Waiting:        1    1   0.5      1       9
Total:          1    2   0.6      2      10

Percentage of the requests served within a certain time (ms)
  50%      2
  66%      2
  75%      2
  80%      2
  90%      2
  95%      2
  98%      3
  99%      3
 100%     10 (longest request)
```
