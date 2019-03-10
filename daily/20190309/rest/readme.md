# rest

Output first nth lines to stderr, and rest to stdout.

```console
$ ps aux | python rest.py | grep bash | sed "s@$USER@ME@g"
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
ME       5274  0.0  0.0  11280  5128 pts/11   Ss+   3月03   0:00 /bin/bash
ME       7152  0.0  0.0  12516  6420 pts/5    Ss+   1月20   0:15 /bin/bash
ME       7362  0.0  0.0  12852  6876 pts/2    Ss+   1月22   0:18 /bin/bash
ME       8086  0.5  0.0  11004  4524 pts/1    Ss   19:00   0:00 /bin/bash --no
ME      20868  0.0  0.0  11388  5200 pts/9    Ss+   1月25   0:01 /bin/bash
ME      21208  0.0  0.0  11280  5180 pts/12   Ss+   3月06   0:00 /bin/bash
ME      27770  0.0  0.0  12320  6164 pts/10   Ss+   1月29   0:15 /bin/bash
ME      27804  0.0  0.0  11144  4696 pts/3    Ss    2月24   0:00 bash
ME      28651  0.0  0.0  12220  6020 pts/8    Ss+   3月03   0:00 /bin/bash
ME      32016  0.0  0.0  12604  6500 pts/4    Ss+   2月09   0:19 /bin/bash
```
