package main

import (
	"os"

	log "github.com/sirupsen/logrus"
)

func main() {
	// Log as JSON instead of the default ASCII formatter.
	log.SetFormatter(&log.TextFormatter{})

	// Output to stdout instead of the default stderr
	// Can be any io.Writer, see below for File example
	log.SetOutput(os.Stdout)

	// // Only log the warning severity or above.
	// log.SetLevel(log.WarnLevel)

	log.WithFields(log.Fields{
		"animal": "walrus",
	}).Debug("A walrus appears")
	log.WithFields(log.Fields{
		"animal": "walrus",
	}).Info("A walrus appears")
	log.WithFields(log.Fields{
		"animal": "walrus",
	}).Warning("A walrus appears")
	log.WithFields(log.Fields{
		"animal":  "walrus",
		"animal2": "walrus",
		"animal3": "walrus",
	}).Error("A walrus appears")
}
