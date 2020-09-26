// +build !api

package main

import (
	"net/http"
)

type Router interface {
	Get(string, http.HandlerFunc)
}
type HelloInteractor interface {
	Hello() string
}
type HelloInteractor struct {
}

func (ir *HelloInteractor) Hello() string {
	return "hello"
}

type Mounter interface {
	MountHello(r Router, ir *HelloInteractor)
}

func Mount(m Mounter, r Router) {
	m.MountHello(r, &HelloInteractor{})
}
