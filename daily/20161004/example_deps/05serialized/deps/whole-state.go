package deps

import (
	analysis "../analysis"
)

// WholeState is x
type WholeState struct {
	sentinel *Sentinel
	States   map[string]State
	Analysis *analysis.Analysis
}

// NewWholeState is x
func NewWholeState(analysis *analysis.Analysis) *WholeState {
	ws := WholeState{States: make(map[string]State), Analysis: analysis}
	return &ws
}
