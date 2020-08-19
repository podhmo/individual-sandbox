package greeting

import (
	"context"
	"fmt"
)

// Deprecated: Hello ...
func Hello() {
	fmt.Println("hello")
}

// HelloWithContext ...
func HelloWithContext(ctx context.Context) {
}
