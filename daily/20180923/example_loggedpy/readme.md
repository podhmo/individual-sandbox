## 00

```console
$ python logged.py 00*.py
INFO	2018-09-23 18:50:44,449	hello
```

## 01

```console
$ python logged.py 01*.py
INFO	2018-09-23 18:51:00,471	hello
```

## 02

```console
$ python logged.py dictknife cat a.json
DEBUG	2018-09-23 18:51:15,213	merge: a.json
name: foo
```

## 03

```console
$ python logged.py -m tokenize logged.py
INFO	2018-09-23 19:13:23,523	0,0-0,0:            ENCODING       'utf-8'        
INFO	2018-09-23 19:13:23,523	1,0-1,6:            NAME           'import'       
INFO	2018-09-23 19:13:23,523	1,7-1,13:           NAME           'typing'       
INFO	2018-09-23 19:13:23,523	1,14-1,16:          NAME           'as'           
INFO	2018-09-23 19:13:23,523	1,17-1,18:          NAME           't'            
INFO	2018-09-23 19:13:23,523	1,18-1,19:          NEWLINE        '\n'           
INFO	2018-09-23 19:13:23,523	2,0-2,6:            NAME           'import'       
INFO	2018-09-23 19:13:23,523	2,7-2,10:           NAME           'sys'          
INFO	2018-09-23 19:13:23,523	2,10-2,11:          NEWLINE        '\n'           
INFO	2018-09-23 19:13:23,523	3,0-3,6:            NAME           'import'       
INFO	2018-09-23 19:13:23,523	3,7-3,9:            NAME           'os'           
INFO	2018-09-23 19:13:23,523	3,9-3,10:           OP             '.'            
INFO	2018-09-23 19:13:23,523	3,10-3,14:          NAME           'path'         
INFO	2018-09-23 19:13:23,523	3,14-3,15:          NEWLINE        '\n'           
...  
INFO	2018-09-23 19:13:23,936	113,8-113,9:        OP             '('            
INFO	2018-09-23 19:13:23,936	113,9-113,10:       OP             ')'            
INFO	2018-09-23 19:13:23,936	113,10-113,11:      NEWLINE        '\n'           
INFO	2018-09-23 19:13:23,936	114,0-114,0:        DEDENT         ''             
INFO	2018-09-23 19:13:23,936	114,0-114,0:        ENDMARKER      ''           
```
