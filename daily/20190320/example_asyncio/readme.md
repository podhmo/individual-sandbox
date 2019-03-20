ok 
```console
$ python runner.py python 00run.py
spwan ['python', '00run.py'] (waittime=10)
DEBUG:asyncio:Using selector: EpollSelector
INFO:__main__:update lifetime:0 23
INFO:__main__:running task 0
INFO:__main__:running task 1
INFO:__main__:running task 2
INFO:__main__:running task 3
INFO:__main__:running task 4
INFO:__main__:running task 5
INFO:__main__:update lifetime:1 23
INFO:__main__:running task 6
INFO:__main__:running task 7
INFO:__main__:running task 8
INFO:__main__:running task 9
INFO:__main__:task completed, result='ok'
DEBUG:asyncio:Close <_UnixSelectorEventLoop running=False closed=False debug=True>
```

ng
```console
NG=1 python runner.py python 00run.py
spwan ['python', '00run.py'] (waittime=10)
DEBUG:asyncio:Using selector: EpollSelector
INFO:__main__:update lifetime:0 23
INFO:__main__:running task 0
INFO:__main__:running task 1
INFO:__main__:running task 2
INFO:__main__:running task 3
INFO:__main__:running task 4
INFO:__main__:running task 5
INFO:__main__:update lifetime:1 23
INFO:__main__:running task 6
INFO:__main__:running task 7
INFO:__main__:running task 8
INFO:__main__:running task 9
INFO:__main__:task completed, result="ng (Exception('hmm'))"
DEBUG:asyncio:Close <_UnixSelectorEventLoop running=False closed=False debug=True>
```

interrupted
```console
python runner.py --waittime=3 python 00run.py
spwan ['python', '00run.py'] (waittime=3)
DEBUG:asyncio:Using selector: EpollSelector
INFO:__main__:update lifetime:0 23
INFO:__main__:running task 0
INFO:__main__:running task 1
INFO:__main__:running task 2
INFO:__main__:running task 3
INFO:__main__:running task 4
INFO:__main__:running task 5
TIMEOUT: Command '['python', '00run.py']' timed out after 3 seconds
DEBUG:asyncio:poll 489.298 ms took 434.045 ms: 1 events
INFO:__main__:task is interrupted (catch SIGINT)
DEBUG:asyncio:Close <_UnixSelectorEventLoop running=False closed=False debug=True>
```
