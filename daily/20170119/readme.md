# makefile shellのoutputの結果を使いたいけれど不要なタスクの部分も実行されてしまう事は避けたい


大丈夫だった。これでdefaultではanotherの中の `$(shell make two)` は実行されない

```make
default:
	echo $(shell make one) times

another:
	echo $(shell make two) times

one:
	@echo one

two:
	@echo two
```

[shell functionのヘルプ](https://www.gnu.org/software/make/manual/html_node/Shell-Function.html)見ても大丈夫そうな感じ。
[wildcard function](https://www.gnu.org/software/make/manual/html_node/Wildcard-Function.html#Wildcard-Function) とか知らなかった。
