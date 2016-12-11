```
$ make setup
$ make test 2>&1 | gsed "s@`pwd`@./@g"
go test -v
=== RUN   TestDayOfX
=== RUN   TestDayOfX/first_day_of_week
=== RUN   TestDayOfX/first_day_of_week/2016-12-14
=== RUN   TestDayOfX/first_day_of_week/2016-12-12
=== RUN   TestDayOfX/first_day_of_week/2016-12-11
=== RUN   TestDayOfX/last_day_of_week
=== RUN   TestDayOfX/last_day_of_week/2016-12-14
=== RUN   TestDayOfX/last_day_of_week/2016-12-18
=== RUN   TestDayOfX/last_day_of_week/2016-12-19
=== RUN   TestDayOfX/first_day_of_month
=== RUN   TestDayOfX/first_day_of_month/2016-12-14
=== RUN   TestDayOfX/first_day_of_month/2016-12-01
=== RUN   TestDayOfX/first_day_of_month/2016-11-30
=== RUN   TestDayOfX/first_day_of_month/2016-10-31
=== RUN   TestDayOfX/last_day_of_month
=== RUN   TestDayOfX/last_day_of_month/2016-12-14
=== RUN   TestDayOfX/last_day_of_month/2016-12-31
=== RUN   TestDayOfX/last_day_of_month/2017-01-01
=== RUN   TestDayOfX/last_day_of_month/2016-11-30
=== RUN   TestDayOfX/last_day_of_month/2016-10-31
=== RUN   TestDayOfX/last_day_of_month/2016-02-29
=== RUN   TestDayOfX/last_day_of_month/2017-02-28
--- PASS: TestDayOfX (0.00s)
    --- PASS: TestDayOfX/first_day_of_week (0.00s)
        --- PASS: TestDayOfX/first_day_of_week/2016-12-14 (0.00s)
        --- PASS: TestDayOfX/first_day_of_week/2016-12-12 (0.00s)
        --- PASS: TestDayOfX/first_day_of_week/2016-12-11 (0.00s)
    --- PASS: TestDayOfX/last_day_of_week (0.00s)
        --- PASS: TestDayOfX/last_day_of_week/2016-12-14 (0.00s)
        --- PASS: TestDayOfX/last_day_of_week/2016-12-18 (0.00s)
        --- PASS: TestDayOfX/last_day_of_week/2016-12-19 (0.00s)
    --- PASS: TestDayOfX/first_day_of_month (0.00s)
        --- PASS: TestDayOfX/first_day_of_month/2016-12-14 (0.00s)
        --- PASS: TestDayOfX/first_day_of_month/2016-12-01 (0.00s)
        --- PASS: TestDayOfX/first_day_of_month/2016-11-30 (0.00s)
        --- PASS: TestDayOfX/first_day_of_month/2016-10-31 (0.00s)
    --- PASS: TestDayOfX/last_day_of_month (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2016-12-14 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2016-12-31 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2017-01-01 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2016-11-30 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2016-10-31 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2016-02-29 (0.00s)
        --- PASS: TestDayOfX/last_day_of_month/2017-02-28 (0.00s)
PASS
ok  	_./	0.013s
```
