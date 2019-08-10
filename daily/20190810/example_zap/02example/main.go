package main

import (
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

func main() {
	{
		l := zap.NewExample(
			zap.AddCaller(),
			zap.AddStacktrace(zapcore.DebugLevel),
		)
		l.Info("hello")
	}
	{
		l := NewExample(
			zap.AddCaller(),
		)
		l.Info("hello")
	}
	{
		l, _ := zap.NewProduction()
		l.Info("hello")
	}
	{
		l, _ := zap.NewDevelopment()
		l.Info("hello")
	}
	{
		l, _ := zap.NewDevelopment(zap.AddStacktrace(zapcore.DebugLevel))
		l.Info("hello")
	}
}

func NewExample(options ...zap.Option) *zap.Logger {
	c := zap.NewProductionConfig()
	c.EncoderConfig.TimeKey = ""
	c.OutputPaths = []string{"stdout"}
	c.ErrorOutputPaths = []string{"stdout"}
	l, _ := c.Build(options...)
	return l
}
