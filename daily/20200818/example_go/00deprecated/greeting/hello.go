package greeting

import (
	"context"
	"fmt"
)

// Hello ...
//
// Deprecated: use HelloWithContext()
func Hello() {
	fmt.Println("hello")
}

// HelloWithContext ...
func HelloWithContext(ctx context.Context) {
}
