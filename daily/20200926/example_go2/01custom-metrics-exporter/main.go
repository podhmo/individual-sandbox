package main

import (
	"context"
	"log"
	"time"

	"go.opencensus.io/stats"
	"go.opencensus.io/stats/view"
	"go.opencensus.io/tag"
)

type customMetricsExporter struct{}

func (ce *customMetricsExporter) ExportView(vd *view.Data) {
	log.Printf("vd.View: %+v\n%#v\n", vd.View, vd.Rows)
	for i, row := range vd.Rows {
		log.Printf("\tRow: %#d: %#v\n", i, row)
	}
	log.Printf("StartTime: %s EndTime: %s\n\n", vd.Start.Round(0), vd.End.Round(0))
}

func main() {
	log.SetFlags(0)
	// We need to have registered at least one view
	if err := view.Register(loopCountView); err != nil {
		log.Fatalf("Failed to register loopCountView: %v", err)
	}

	// Please remember to register your exporter
	// so that it can receive exported view Data.
	view.RegisterExporter(new(customMetricsExporter))

	// For demo purposes we'll use a very short view data reporting
	// period so that we can examine the stats send to our exporter, quickly.
	view.SetReportingPeriod(100 * time.Millisecond)

	ctx, _ := tag.New(context.Background(), tag.Upsert(keyMethod, "main"))
	for i := int64(0); i < 5; i++ {
		stats.Record(ctx, mLoops.M(i))
		<-time.After(10 * time.Millisecond)
	}
	<-time.After(500 * time.Millisecond)
}

// The measure and view to be used for demo purposes
var keyMethod, _ = tag.NewKey("method")
var mLoops = stats.Int64("demo/loop_iterations", "The number of loop iterations", "1")
var loopCountView = &view.View{
	Measure: mLoops, Name: "demo/loop_iterations",
	Description: "Number of loop iterations",
	Aggregation: view.Count(),
	TagKeys:     []tag.Key{keyMethod},
}
