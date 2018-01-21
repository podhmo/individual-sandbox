## gomockとtestify/mockのどちらが良いか

結論から言うと常にtestify/mockで良い気がした。

gomockにはmockgenがあった(しかし現在ではtestify/mockにもmockeryがある)。


特にgomockでは関心外の領域でdummy objectとして利用したくなった時に不便。(mockの生成に*testing.Tを受け取る必要があるのが面倒。gomockを忘れることができないのが面倒)

あと、gomockは登録とasserttionが不可分なところがちょっと嫌(実際にはdeferでFinishを呼ばないということはできるけれど嫌な習慣)。一方でtesitfy/mockはassertionを呼ばなければ良いだけなので手軽(細かい話をするとassertionとfakeを兼ねたいときと兼ねたくない場合がありfakeだけ行いたい場合もある)。

```console
$ make setup
go get -v github.com/golang/mock/...
go get -v github.com/vektra/mockery/...
golang.org/x/tools/go/loader
github.com/vektra/mockery/mockery
github.com/vektra/mockery/cmd/mockery

$ make gen
mkdir -p gengomock
mockgen -source hello/hello.go -package gengomock -destination ./gengomock/hello.go
mkdir -p genmock
mockery -all -dir ./hello -outpkg genmock -output ./genmock/
Generating mock for: Hello

$ make test
go test -v
=== RUN   TestGoMock
=== RUN   TestGoMock/called
=== RUN   TestGoMock/not_called
--- PASS: TestGoMock (0.00s)
    --- PASS: TestGoMock/called (0.00s)
    --- PASS: TestGoMock/not_called (0.00s)
=== RUN   TestGoMock2
=== RUN   TestGoMock2/called,_but_it_is_not_main_concern
--- PASS: TestGoMock2 (0.00s)
    --- PASS: TestGoMock2/called,_but_it_is_not_main_concern (0.00s)
=== RUN   TestTestifyMock
=== RUN   TestTestifyMock/called
=== RUN   TestTestifyMock/not_called
--- PASS: TestTestifyMock (0.00s)
    --- PASS: TestTestifyMock/called (0.00s)
    --- PASS: TestTestifyMock/not_called (0.00s)
=== RUN   TestTestifyMock2
=== RUN   TestTestifyMock2/called,_but_it_is_not_main_concern
--- PASS: TestTestifyMock2 (0.00s)
    --- PASS: TestTestifyMock2/called,_but_it_is_not_main_concern (0.00s)
PASS
ok
```
