package main

import (
	"go/ast"
	"go/build"
	"log"
)

type message struct {
	pkg  string
	file <-chan *ast.File
}

type worker struct {
	id    int
	ch    chan *message
	build *build.Context
}

func (w *worker) load() {
}

// Package :
type Package struct {
	files map[string]<-chan *ast.File
}

type loader struct {
	packages    map[string]<-chan *ast.Package
	workers     []*worker
	concurrency int
}

func newLoader(concurrency int) *loader {
	return &loader{
		packages:    map[string]<-chan *ast.Package{},
		workers:     []*worker{},
		concurrency: concurrency,
	}
}

func (l *loader) Import(pkg string) <-chan *Package {
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	return nil
}
