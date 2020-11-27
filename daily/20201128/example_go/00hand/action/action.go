package action

import (
	"context"
	"fmt"
	"m/00hand/apperror"
)

type HelloInput struct {
	Name string `json:"name"`
}

func Hello(ctx context.Context, input HelloInput, short *bool) (string, error) {
	name := input.Name
	if short != nil && *short {
		return fmt.Sprintf("Hi %s", name), nil
	}
	return fmt.Sprintf("Hello %s", name), nil
}

func IsEven(ctx context.Context, v int) (string, error) {
	if v%2 == 0 {
		return "ok", nil
	}
	return "ng", apperror.New(fmt.Errorf("not even %+v", v), 400)
}
