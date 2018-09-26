## 00

```console
$ python logged.py 00*.py
INFO	2018-09-23 23:01:15,370	hello
```

## 01

```console
$ python logged.py 01*.py
INFO	2018-09-23 23:01:54,645	hello
INFO	2018-09-23 23:01:54,646	hello
INFO	2018-09-23 23:01:54,646	hello
```

## 02

```console
$ python logged.py dictknife cat a.json
DEBUG	2018-09-23 23:02:18,508	merge: a.json
name: foo
```

## 03

```console
$ python logged.py -m tokenize logged.py
INFO	2018-09-23 23:02:38,300	0,0-0,0:            ENCODING       'utf-8'        
INFO	2018-09-23 23:02:38,301	1,0-1,6:            NAME           'import'       
INFO	2018-09-23 23:02:38,301	1,7-1,13:           NAME           'typing'       
INFO	2018-09-23 23:02:38,301	1,14-1,16:          NAME           'as'           
INFO	2018-09-23 23:02:38,301	1,17-1,18:          NAME           't'            
INFO	2018-09-23 23:02:38,301	1,18-1,19:          NEWLINE        '\n'           
INFO	2018-09-23 23:02:38,301	2,0-2,6:            NAME           'import'       
INFO	2018-09-23 23:02:38,301	2,7-2,10:           NAME           'sys'          
INFO	2018-09-23 23:02:38,302	2,10-2,11:          NEWLINE        '\n'           
INFO	2018-09-23 23:02:38,302	3,0-3,6:            NAME           'import'       
INFO	2018-09-23 23:02:38,302	3,7-3,9:            NAME           'os'           
...
INFO	2018-09-23 23:02:39,243	108,15-108,25:      STRING         '"__main__"'   
INFO	2018-09-23 23:02:39,243	108,25-108,26:      OP             ':'            
INFO	2018-09-23 23:02:39,243	108,26-108,27:      NEWLINE        '\n'           
INFO	2018-09-23 23:02:39,243	109,0-109,4:        INDENT         '    '         
INFO	2018-09-23 23:02:39,243	109,4-109,8:        NAME           'main'         
INFO	2018-09-23 23:02:39,244	109,8-109,9:        OP             '('            
INFO	2018-09-23 23:02:39,244	109,9-109,10:       OP             ')'            
INFO	2018-09-23 23:02:39,286	109,10-109,11:      NEWLINE        '\n'           
INFO	2018-09-23 23:02:39,286	110,0-110,0:        DEDENT         ''             
INFO	2018-09-23 23:02:39,286	110,0-110,0:        ENDMARKER      ''             
```

## 04

```console
$ python logged.py --loggedpy-driver=./customized.py:Driver 00*.py
INFO	2018-09-23 23:03:28,897	__main__	hello	in	00hello.py:1	<module>
```

## 05

```console
$ python logged.py --loggedpy-driver=./customized.py:Driver 01*.py
INFO	2018-09-23 23:03:40,472	__main__	hello	in	01hello.py:2	hello
INFO	2018-09-23 23:03:40,472	__main__	hello	in	01hello.py:2	hello
INFO	2018-09-23 23:03:40,472	__main__	hello	in	01hello.py:2	hello
```

## 06

```console
$ python logged.py --loggedpy-driver=./customized.py:Driver dictknife cat a.json
DEBUG	2018-09-23 23:03:53,981	dictknife.commands.dictknife	merge: a.json	in	dictknife.py:43	cat
name: foo
```

## 07

```console
$ python logged.py --loggedpy-driver=./customized.py:Driver -m tokenize logged.py
INFO	2018-09-23 23:04:09,014	tokenize	0,0-0,0:            ENCODING       'utf-8'        	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	1,0-1,6:            NAME           'import'       	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	1,7-1,13:           NAME           'typing'       	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	1,14-1,16:          NAME           'as'           	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	1,17-1,18:          NAME           't'            	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	1,18-1,19:          NEWLINE        '\n'           	in	tokenize.py:708	main
INFO	2018-09-23 23:04:09,014	tokenize	2,0-2,6:            NAME           'import'       	in	tokenize.py:708	main
...
INFO	2018-09-23 23:04:10,832	tokenize	108,3-108,11:       NAME           '__name__'     	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,832	tokenize	108,12-108,14:      OP             '=='           	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,832	tokenize	108,15-108,25:      STRING         '"__main__"'   	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,832	tokenize	108,25-108,26:      OP             ':'            	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,832	tokenize	108,26-108,27:      NEWLINE        '\n'           	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,832	tokenize	109,0-109,4:        INDENT         '    '         	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	109,4-109,8:        NAME           'main'         	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	109,8-109,9:        OP             '('            	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	109,9-109,10:       OP             ')'            	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	109,10-109,11:      NEWLINE        '\n'           	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	110,0-110,0:        DEDENT         ''             	in	tokenize.py:708	main
INFO	2018-09-23 23:04:10,833	tokenize	110,0-110,0:        ENDMARKER      ''             	in	tokenize.py:708	main
```
