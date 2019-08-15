package zapctxlog2_test

import (
	"strconv"
	"testing"

	"github.com/podhmo/ctxlog/zapctxlog"
	"github.com/podhmo/ctxlog/zapctxlog/zapctxlog2"
)

func Benchmark_ZapCtxlog(b *testing.B) {
	l := zapctxlog.MustNew()
	for i := 0; i < 10000; i++ {
		l = l.With(strconv.Itoa(i), i)
	}
	_ = l
}

func Benchmark_ZapCtxlog2(b *testing.B) {
	l := zapctxlog2.MustNew()
	for i := 0; i < 10000; i++ {
		l = l.With(strconv.Itoa(i), i)
	}
	_ = l
}
