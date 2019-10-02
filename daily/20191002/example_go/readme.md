```
func (c *T) Name() string
    Name returns the name of the running test or benchmark.

func (t *T) Parallel()
    Parallel signals that this test is to be run in parallel with (and only
    with) other parallel tests. When a test is run multiple times due to use
    of -test.count or -test.cpu, multiple instances of a single test never
    run in parallel with each other.

func (t *T) Run(name string, f func(t *T)) bool
    Run runs f as a subtest of t called name. It runs f in a separate
    goroutine and blocks until f returns or calls t.Parallel to become a
    parallel test. Run reports whether f succeeded (or at least did not fail
    before calling t.Parallel).

    Run may be called simultaneously from multiple goroutines, but all such
    calls must return before the outer test function for t returns.

func (c *T) Skip(args ...interface{})
    Skip is equivalent to Log followed by SkipNow.

func (c *T) SkipNow()
    SkipNow marks the test as having been skipped and stops its execution by
    calling runtime.Goexit. If a test fails (see Error, Errorf, Fail) and is
    then skipped, it is still considered to have failed. Execution will
    continue at the next test or benchmark. See also FailNow. SkipNow must
    be called from the goroutine running the test, not from other goroutines
    created during the test. Calling SkipNow does not stop those other
    goroutines.

func (c *T) Skipf(format string, args ...interface{})
    Skipf is equivalent to Logf followed by SkipNow.

func (c *T) Skipped() bool
    Skipped reports whether the test was skipped.
```

こういう定義になっているので素直に全部は取れない。

```go
type T struct {
	common
	isParallel bool
	context    *testContext // For running tests and subtests.
}
```

testing.commonの方のメソッドとtesting.Tの方のメソッドがある。

```console
$ make 02
# ...
testing /usr/lib/go/src/testing/testing.go
	*T	Parallel
	*T	Run
	*T	report
```
