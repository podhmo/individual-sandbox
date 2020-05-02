package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"go.uber.org/fx"
)

type X struct {
}
type Y struct {
	X *X
}
type Z struct {
	Y *Y
}

func NewX() *X {
	return &X{}
}
func NewY(x *X) (*Y, error) {
	return &Y{X: x}, nil
}
func NewZ(y *Y) *Z {
	return &Z{Y: y}
}

func Run(y *Y, z *Z) {
	fmt.Println(y, z)
}
func main() {
	app := fx.New(
		fx.Provide(NewX, NewY, NewZ),
		fx.Invoke(Run),
	)
	ctx := context.Background()
	if err := app.Start(ctx); err != nil {
		log.Fatalf("! +%v", err)
	}

	stopCtx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	if err := app.Stop(stopCtx); err != nil {
		log.Fatalf("!! +%v", err)
	}
}
