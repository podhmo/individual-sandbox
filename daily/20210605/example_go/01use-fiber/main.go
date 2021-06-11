package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/signal"
	"strconv"
	"syscall"
	"time"

	"github.com/gofiber/fiber/v2"
	"github.com/gofiber/fiber/v2/middleware/logger"
	"github.com/gofiber/fiber/v2/middleware/pprof"
	"github.com/gofiber/fiber/v2/middleware/recover"
	"github.com/gofiber/fiber/v2/middleware/requestid"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	ctx := context.Background()

	bc := NewDefaultBuildContext()
	runner := bc.Build()
	return runner.Run(ctx)
}

func Mount(bc *BuildContext, app *fiber.App) *fiber.App {
	// DI if need
	app.Get("/", hello())
	app.Get("/_panic", doPanic())
	return app
}

func hello() fiber.Handler {
	return func(c *fiber.Ctx) error {
		return c.JSON(map[string]interface{}{"message": "Hello world!"})
	}
}
func doPanic() fiber.Handler {
	return func(c *fiber.Ctx) error {
		panic("hmm")
	}
}

type BuildContext struct {
	ReadHeaderTimeout time.Duration
	IdleTimeout       time.Duration

	Port int
}

func NewDefaultBuildContext() *BuildContext {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}

	return &BuildContext{
		Port:              port,
		ReadHeaderTimeout: 5 * time.Second,
		IdleTimeout:       5 * time.Second,
	}
}

func (bc *BuildContext) Build() *Runner {
	app := fiber.New(fiber.Config{
		IdleTimeout: bc.IdleTimeout,
	})

	app.Use(pprof.New())
	app.Use(requestid.New())
	app.Use(logger.New(logger.Config{
		Format: "${pid} ${locals:requestid} ${status} - ${method} ${path}\n",
	}))
	app.Use(recover.New())

	Mount(bc, app)

	return &Runner{
		bc:  bc,
		App: app,
	}
}

type Runner struct {
	bc *BuildContext

	App *fiber.App
}

func (runner *Runner) Teardown(err *error) {
	fmt.Println("Running cleanup tasks...")

	// Your cleanup tasks go here
	// db.Close()
	// redisConn.Close()

	// *err = fmt.Errorf("hmm")

	fmt.Println("Fiber was successful shutdown.")
}

func (runner *Runner) Run(ctx context.Context) (retErr error) {
	bc := runner.bc
	app := runner.App
	defer runner.Teardown(&retErr)

	log.Println("listen ...", bc.Port)
	addr := fmt.Sprintf(":%d", bc.Port)

	// Listen from a different goroutine
	go func() {
		if err := app.Listen(addr); err != nil {
			log.Panic(err)
		}
	}()

	c := make(chan os.Signal, 1)                    // Create channel to signify a signal being sent
	signal.Notify(c, os.Interrupt, syscall.SIGTERM) // When an interrupt or termination signal is sent, notify the channel

	_ = <-c // This blocks the main thread until an interrupt is received
	fmt.Println("Gracefully shutting down...")
	return app.Shutdown()
}
