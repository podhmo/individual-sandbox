package main

/*
typedef struct {
  int w;
  int h;
} SizeInfo;
*/
import "C"

import (
	"fmt"
	"image"
	_ "image/gif"
	_ "image/jpeg"
	_ "image/png"
	"os"
)

func main() {
}

func size(filename string) (w int, h int) {
	f, err := os.Open(filename)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 0, 0
	}

	im, _, err := image.DecodeConfig(f)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return 0, 0
	}
	return im.Width, im.Height
}

// Size :
//export Size
func Size(filename *C.char) (w C.int, h C.int) {
	gw, gh := size(C.GoString(filename))
	return C.int(gw), C.int(gh)
}
