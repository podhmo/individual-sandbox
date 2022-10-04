package main

import (
	"fmt"
	"os"
	"reflect"
	"runtime"
	"unsafe"
	_ "unsafe"
)

type S struct{}

//go:noinline
func (s *S) Hello() {
	println("Hello")
}

//go:linkname runtime_findmoduledatap runtime.findmoduledatap
func runtime_findmoduledatap(pc uintptr) *moduledata

type pcHeader struct {
	magic          uint32  // 0xFFFFFFF0
	pad1, pad2     uint8   // 0,0
	minLC          uint8   // min instruction size
	ptrSize        uint8   // size of a ptr in bytes
	nfunc          int     // number of functions in the module
	nfiles         uint    // number of entries in the file tab
	textStart      uintptr // base for function entry PC offsets in this module, equal to moduledata.text
	funcnameOffset uintptr // offset to the funcnametab variable from pcHeader
	cuOffset       uintptr // offset to the cutab variable from pcHeader
	filetabOffset  uintptr // offset to the filetab variable from pcHeader
	pctabOffset    uintptr // offset to the pctab variable from pcHeader
	pclnOffset     uintptr // offset to the pclntab variable from pcHeader
}

type functab struct {
	entryoff uint32 // relative to runtime.text
	funcoff  uint32
}

type textsect struct {
	vaddr    uintptr // prelinked section vaddr
	end      uintptr // vaddr + section length
	baseaddr uintptr // relocated section address
}

type moduledata struct {
	pcHeader     *pcHeader
	funcnametab  []byte
	cutab        []uint32
	filetab      []byte
	pctab        []byte
	pclntable    []byte
	ftab         []functab
	findfunctab  uintptr
	minpc, maxpc uintptr

	text, etext           uintptr
	noptrdata, enoptrdata uintptr
	data, edata           uintptr
	bss, ebss             uintptr
	noptrbss, enoptrbss   uintptr
	end, gcdata, gcbss    uintptr
	types, etypes         uintptr
	rodata                uintptr
	gofunc                uintptr // go.func.*

	textsectmap []textsect
	typelinks   []int32 // offsets from types
	itablinks   []*itab

	ptab []ptabEntry

	pluginpath string
	pkghashes  []modulehash

	modulename   string
	modulehashes []modulehash

	hasmain uint8 // 1 if module contains the main function, 0 otherwise

	gcdatamask, gcbssmask bitvector

	typemap map[typeOff]*_type // offset to *_rtype in previous module

	bad bool // module failed to load and should be ignored

	next *moduledata
}

type nameOff int32
type typeOff int32
type textOff int32

type bitvector struct {
	n        int32 // # of bits
	bytedata *uint8
}

type modulehash struct {
	modulename   string
	linktimehash string
	runtimehash  *string
}

type interfacetype struct {
	typ     _type
	pkgpath name
	mhdr    []imethod
}

type imethod struct {
	name nameOff
	ityp typeOff
}

type name struct {
	bytes *byte
}

type tflag uint8

const (
	tflagUncommon      tflag = 1 << 0
	tflagExtraStar     tflag = 1 << 1
	tflagNamed         tflag = 1 << 2
	tflagRegularMemory tflag = 1 << 3 // equal and hash can treat values of this type as a single region of t.size bytes
)

type _type struct {
	size       uintptr
	ptrdata    uintptr // size of memory prefix holding all pointers
	hash       uint32
	tflag      tflag
	align      uint8
	fieldAlign uint8
	kind       uint8
	// function for comparing objects of this type
	// (ptr to object A, ptr to object B) -> ==?
	equal func(unsafe.Pointer, unsafe.Pointer) bool
	// gcdata stores the GC type data for the garbage collector.
	// If the KindGCProg bit is set in kind, gcdata is a GC program.
	// Otherwise it is a ptrmask bitmap. See mbitmap.go for details.
	gcdata    *byte
	str       nameOff
	ptrToThis typeOff
}

type ptabEntry struct {
	name nameOff
	typ  typeOff
}

type itab struct {
	inter *interfacetype
	_type *_type
	hash  uint32 // copy of _type.hash. Used for type switches.
	_     [4]byte
	fun   [1]uintptr // variable sized. fun[0]==0 means _type does not implement intfindfunctab
}

func main() {
	ob := &S{}
	ob.Hello() // Hello-fm
	println(ob.Hello)
	fmt.Println(reflect.ValueOf(ob.Hello).Pointer())
	{
		rfunc := runtime.FuncForPC(reflect.ValueOf(ob.Hello).Pointer())
		fmt.Println(rfunc.FileLine(rfunc.Entry()))
	}
	{
		m := runtime_findmoduledatap(reflect.ValueOf(ob.Hello).Pointer())

		// strings <(go run main.go)
		// os.Stdout.Write(m.filetab)
		os.Stdout.Write(m.funcnametab)

		// pcln := gosym.NewLineTable(m.pclntable, uint64(m.text))
		// tab, err := gosym.NewTable(m.symdat, pcln)
	}
}
