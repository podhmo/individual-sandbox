package internal

import (
	"context"
	"errors"
	"math"

	"github.com/semrush/zenrpc"
)

type ArithService struct{ zenrpc.Service }

// Sum sums two digits and returns error with error code as result and IP from context.
func (as ArithService) Sum(ctx context.Context, a, b int) (int, *zenrpc.Error) {
	r, _ := zenrpc.RequestFromContext(ctx)

	return a+b, zenrpc.NewStringError(a+b, r.Host)
}

// Multiply multiples two digits and returns result.
func (as ArithService) Multiply(a, b int) int {
	return a * b
}

type Quotient struct {
	Quo, Rem int
}

func (as ArithService) Divide(a, b int) (quo *Quotient, err error) {
	if b == 0 {
		return nil, errors.New("divide by zero")
	} else if b == 1 {
		return nil, zenrpc.NewError(401, errors.New("we do not serve 1"))
	}

	return &Quotient{
		Quo: a / b,
		Rem: a % b,
	}, nil
}

// Pow returns x**y, the base-x exponential of y. If Exp is not set then default value is 2.
//zenrpc:exp=2
func (as ArithService) Pow(base float64, exp float64) float64 {
	return math.Pow(base, exp)
}

//go:generate go run github.com/semrush/zenrpc/zenrpc
