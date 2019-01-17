package main

import "fmt"

// DryRun :
type DryRun interface {
	DryRun() bool
	SetDryRun(bool)
}

// DryRunHandler :
type DryRunHandler struct {
	dryRun bool
}

// SetDryRun is a setter for dry run option.
func (h *DryRunHandler) SetDryRun(dryRun bool) {
	h.dryRun = dryRun
}

// DryRun is a setter for dry run option.
func (h *DryRunHandler) DryRun() bool {
	return h.dryRun
}

// Handler :
type Handler struct {
	DryRunHandler
}

func main() {
	h := Handler{}
	fmt.Println(h.DryRun())
	h.SetDryRun(true)
	fmt.Println(h.DryRun())
}
