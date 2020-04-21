```
go test -v
=== RUN   TestParse
=== RUN   TestParse/["-name"_"foo"]
=== RUN   TestParse/["--name"_"foo"]
=== RUN   TestParse/["-name=foo"]
=== RUN   TestParse/["--name=foo"]
--- PASS: TestParse (0.00s)
    --- PASS: TestParse/["-name"_"foo"] (0.00s)
    --- PASS: TestParse/["--name"_"foo"] (0.00s)
    --- PASS: TestParse/["-name=foo"] (0.00s)
    --- PASS: TestParse/["--name=foo"] (0.00s)
=== RUN   TestParseDefault
--- PASS: TestParseDefault (0.00s)
=== RUN   TestHelp
=== RUN   TestHelp/["-h"]
=== RUN   TestHelp/["--help"]
--- PASS: TestHelp (0.00s)
    --- PASS: TestHelp/["-h"] (0.00s)
    --- PASS: TestHelp/["--help"] (0.00s)
PASS
ok  	m/01parse	0.007s
```
