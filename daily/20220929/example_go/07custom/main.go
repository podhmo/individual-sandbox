package main

import (
	"bytes"
	"errors"
	"io"
	"io/fs"
	"log"
	"net/http"
	"os"
	"strings"
)

func main() {
	d := os.Args[1]
	root := ioFS{os.DirFS(d)}
	http.Handle("/downloads/", http.StripPrefix("/downloads/", http.FileServer(root)))

	log.Println("listen", 8080)
	if err := http.ListenAndServe("localhost:8080", nil); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type ioFS struct {
	fsys fs.FS
}

type ioFile struct {
	file fs.File
}

func (f ioFS) Open(name string) (http.File, error) {
	if name == "/" {
		name = "."
	} else {
		name = strings.TrimPrefix(name, "/")
	}
	file, err := f.fsys.Open(name)
	if err != nil {
		return nil, mapOpenError(err, name, '/', func(path string) (fs.FileInfo, error) {
			return fs.Stat(f.fsys, path)
		})
	}
	return ioFile{file}, nil
}

func (f ioFile) Close() error               { return f.file.Close() }
func (f ioFile) Read(b []byte) (int, error) { return f.file.Read(b) }
func (f ioFile) Stat() (fs.FileInfo, error) { return f.file.Stat() }

var errMissingSeek = errors.New("io.File missing Seek method")
var errMissingReadDir = errors.New("io.File directory missing ReadDir method")

func (f ioFile) Seek(offset int64, whence int) (int64, error) {
	s, ok := f.file.(io.Seeker)
	if !ok {
		return 0, errMissingSeek
	}
	return s.Seek(offset, whence)
}

func (f ioFile) ReadDir(count int) ([]fs.DirEntry, error) {
	d, ok := f.file.(fs.ReadDirFile)
	if !ok {
		return nil, errMissingReadDir
	}
	return d.ReadDir(count)
}

func (f ioFile) Readdir(count int) ([]fs.FileInfo, error) {
	d, ok := f.file.(fs.ReadDirFile)
	if !ok {
		return nil, errMissingReadDir
	}
	var list []fs.FileInfo
	for {
		dirs, err := d.ReadDir(count - len(list))
		for _, dir := range dirs {
			info, err := dir.Info()
			if err != nil {
				// Pretend it doesn't exist, like (*os.File).Readdir does.
				continue
			}
			list = append(list, info)
		}
		if err != nil {
			return list, err
		}
		if count < 0 || len(list) >= count {
			break
		}
	}
	return list, nil
}

// mapOpenError maps the provided non-nil error from opening name
// to a possibly better non-nil error. In particular, it turns OS-specific errors
// about opening files in non-directories into fs.ErrNotExist. See Issues 18984 and 49552.
func mapOpenError(originalErr error, name string, sep rune, stat func(string) (fs.FileInfo, error)) error {
	if errors.Is(originalErr, fs.ErrNotExist) || errors.Is(originalErr, fs.ErrPermission) {
		return originalErr
	}

	parts := strings.Split(name, string(sep))
	for i := range parts {
		if parts[i] == "" {
			continue
		}
		fi, err := stat(strings.Join(parts[:i+1], string(sep)))
		if err != nil {
			return originalErr
		}
		if !fi.IsDir() {
			return fs.ErrNotExist
		}
	}
	return originalErr
}

type wFile struct {
	f   *os.File
	buf bytes.Buffer
	err error
}

func (w *wFile) Close() error {
	return w.f.Close()
}

func (w *wFile) Read(p []byte) (n int, err error) {
	return w.f.Read(p)
}

func (w *wFile) Seek(offset int64, whence int) (int64, error) {
	return w.f.Seek(offset, whence)
}

func (w *wFile) Readdir(count int) ([]fs.FileInfo, error) {
	return w.f.Readdir(count)
}

func (w *wFile) Stat() (fs.FileInfo, error) {
	return w.f.Stat()
}
