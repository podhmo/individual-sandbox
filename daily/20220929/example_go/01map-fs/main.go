package main

import (
	"fmt"
	"io/fs"
	"log"
	"os"
	"time"
)

func main() {
	if err := run(); err != nil {
		log.Printf("!! %+v", err)
	}
}

// broken
func run() error {
	root := FS(
		Dir("foo",
			Dir("bar", File("x", nil), File("y", nil), File("z", nil)),
			Dir("boo", File("i", nil), File("j", nil))),
		File("main.go", nil),
	)

	return fs.WalkDir(root, "foo", func(path string, d fs.DirEntry, err error) error {
		fmt.Printf("%T\t%s\t%s\n", d, path, d)
		return nil
	})
}

func FS(files ...interface {
	Name() string
	fs.File
}) *FakeFS {
	return &FakeFS{files: files}
}

type FakeFS struct {
	files []interface {
		Name() string
		fs.File
	}
}

func (root *FakeFS) Open(name string) (fs.File, error) {
	for _, child := range root.files {
		if child.Name() == name {
			return child.(fs.File), nil
		}
	}
	return nil, &fs.PathError{Op: "Open", Path: name, Err: os.ErrNotExist}
}

func Dir(name string, files ...interface {
	fs.File
	Name() string
}) *dir {
	d := &dir{children: files, info: &info{name: name, size: 0, isDir: true}}
	return d
}

func File(name string, body []byte) *file {
	return &file{body: body, info: &info{name: name, size: int64(len(body))}}
}

type file struct {
	*info
	body []byte
}

var _ fs.File = (*file)(nil)
var _ fmt.Stringer = (*file)(nil)

func (f *file) String() string {
	return fmt.Sprintf("File[%s]", f.info.name)
}

func (f *file) Stat() (fs.FileInfo, error) {
	return f.info, nil
}

func (f *file) Read(b []byte) (int, error) {
	return len(b), nil
}

func (f *file) Close() error {
	return nil
}

type dir struct {
	*info
	children []interface {
		fs.File
		Name() string
	}
}

var _ fs.ReadDirFile = (*dir)(nil)
var _ fmt.Stringer = (*dir)(nil)

func (d *dir) String() string {
	return fmt.Sprintf(" Dir[%s]", d.info.name)
}

func (d *dir) Stat() (fs.FileInfo, error) {
	return d.info, nil
}

func (d *dir) Read(b []byte) (int, error) {
	return len(b), nil
}

func (d *dir) Close() error {
	return nil
}

// ReadDir reads the contents of the directory and returns
// a slice of up to n DirEntry values in directory order.
// Subsequent calls on the same file will yield further DirEntry values.
//
// If n > 0, ReadDir returns at most n DirEntry structures.
// In this case, if ReadDir returns an empty slice, it will return
// a non-nil error explaining why.
// At the end of a directory, the error is io.EOF.
//
// If n <= 0, ReadDir returns all the DirEntry values from the directory
// in a single slice. In this case, if ReadDir succeeds (reads all the way
// to the end of the directory), it returns the slice and a nil error.
// If it encounters an error before the end of the directory,
// ReadDir returns the DirEntry list read until that point and a non-nil error.
func (d *dir) ReadDir(n int) ([]fs.DirEntry, error) {
	if n > 0 {
		if n <= len(d.children) {
			r := make([]fs.DirEntry, n)
			for i, child := range d.children {
				if i == n {
					break
				}
				switch x := child.(type) {
				case *file:
					r[i] = x
				case *dir:
					r[i] = x
				default:
					panic(fmt.Sprintf("unexpected type %T", x))
				}
			}
			return r, nil
		}
		panic("not implemented")
	}
	r := make([]fs.DirEntry, len(d.children))
	for i, child := range d.children {
		switch x := child.(type) {
		case *file:
			r[i] = x
		case *dir:
			r[i] = x
		default:
			panic(fmt.Sprintf("unexpected type %T", x))
		}
	}
	return r, nil
}

type info struct {
	name  string
	size  int64
	isDir bool
}

var _ fs.FileInfo = (*info)(nil)
var _ fs.DirEntry = (*info)(nil)

func (fi *info) Name() string       { return fi.name }
func (fi *info) Size() int64        { return fi.size }
func (fi *info) Mode() fs.FileMode  { return 0744 }
func (fi *info) ModTime() time.Time { return time.Now() }
func (fi *info) IsDir() bool        { return fi.isDir }
func (fi *info) Sys() any           { return nil }

func (fi *info) Info() (fs.FileInfo, error) { return fi, nil }
func (fi *info) Type() fs.FileMode          { return 0744 }
