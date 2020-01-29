package main

import (
	"m/02branch/branch"
)

func main() {
	branch.New().Do(branch.XTypeFoo)
	branch.New().Do(branch.XTypeBoo)
}
