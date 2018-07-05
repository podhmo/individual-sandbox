```console
python 02membench.py
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:00  0.0  0.0  70188 14508
before write
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:04 98.4  0.0  70844 15152
----------------------------------------
after write
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:04 98.6  0.0  70452 14760
----------------------------------------
load json
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:05  103  0.7 182776 126888
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:05  104  0.7 179192 123544
----------------------------------------
load dataclass
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:06  100  0.7 185336 129672
  PID     TIME %CPU %MEM    VSZ   RSS
11454 00:00:06  100  0.7 179192 123544
```
