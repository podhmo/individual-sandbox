package main

import (
	"context"
	"fmt"
	"time"

	"go.opencensus.io/trace"
)

type customTraceExporter struct{}

func (ce *customTraceExporter) ExportSpan(sd *trace.SpanData) {
	fmt.Printf("Name: %s\nTraceID: %x\nSpanID: %x\nParentSpanID: %x\nStartTime: %s\nEndTime: %s\nAnnotations: %+v\n\n",
		sd.Name, sd.TraceID, sd.SpanID, sd.ParentSpanID, sd.StartTime, sd.EndTime, sd.Annotations)
}

func main() {
	trace.ApplyConfig(trace.Config{DefaultSampler: trace.AlwaysSample()})

	// Please remember to register your exporter
	// so that it can receive exported spanData.
	trace.RegisterExporter(new(customTraceExporter))

	for i := 0; i < 5; i++ {
		_, span := trace.StartSpan(context.Background(), fmt.Sprintf("sample-%d", i))
		span.Annotate([]trace.Attribute{trace.Int64Attribute("invocations", 1)}, "Invoked it")
		span.End()
		<-time.After(10 * time.Millisecond)
	}
	<-time.After(500 * time.Millisecond)
}
