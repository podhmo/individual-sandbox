#[golang][memo]go/types内で特定のobjectの保持しているメソッドの一覧を取得する方法


go/types内で特定のobjectの保持しているメソッドの一覧を取得する方法のメモ。

以下を知っておけば良い。

1. scope.Lookup(name)でtypes.Objectが取れる
2. structと定義されているmethodの一覧の組はtypes.Namedというstructで管理されている
3. types.Namedから全てのメソッドの一覧を取得するのは簡単

## scope.Lookup(name)でtypes.Objectが取れる

scope.Lookup(name)でtypes.Objectが取れる。例えばbytes.Bufferが取りたい場合には、bytesのtypes.Packageで以下の様にして取れる。
(存在しない場合にはnilが返る)

```go
pkg.Scope().Lookup("Buffer")
```

ここで取れるのは以下の様なinterfaceを満たしたもの

```go
type Object interface {
    Parent() *Scope // scope in which this object is declared; nil for methods and struct fields
    Pos() token.Pos // position of object identifier in declaration
    Pkg() *Package  // package to which this object belongs; nil for labels and objects in the Universe scope
    Name() string   // package local object name
    Type() Type     // object type
    Exported() bool // reports whether the name starts with a capital letter
    Id() string     // object name if exported, qualified name if not exported (see func Id)

    // String returns a human-readable string of the object.
    String() string
    // contains filtered or unexported methods
}
    An Object describes a named language entity such as a package, constant,
    type, variable, function (incl. methods), or label. All objects
    implement the Object interface.
```

## structと定義されているmethodの一覧の組はtypes.Namedというstructで管理されている

structと定義されているmethodの一覧の組はtypes.Namedというstructで管理されている。一瞬classと同じ気持ちでstruct的な値(そしておそらくそれをtypes.structと解釈する)が持っていると錯覚してしまうが。よく考えてみれば、go言語の意味的に、structがmethodを持っているのではなく、method定義はstructと対応する関数のマッピングの定義なので、methodの情報は外側にもつ必要がある。それがtypes.Named。

ちなみにtypes.Namedもtypes.Objectを満たしている。

```go
// A Named represents a named type.
type Named struct {
	obj        *TypeName // corresponding declared object
	underlying Type      // possibly a *Named during setup; never a *Named once set up completely
	methods    []*Func   // methods declared for this type (not the method set of this type)
}
```


そんなわけで以下の様にすればtypes.Named型が取れる。

```go
if named, _ := pkg.Scope().Lookup("Buffer"); named != nil {
	// do something
}
```

## types.Namedから全てのメソッドの一覧を取得するのは簡単

types.Namedから全てのメソッドの一覧を取得するのは簡単。MethodというメソッドとNumMethodsというメソッドが用意されているので。

```go
if named, _ := pkg.Scope().Lookup("Buffer"); named != nil {
	for i := 0; i < named.NumMethods(); i++ {
		fmt.Println(named.Method(i)) // *types.Func
	}
}
```

## 全部つなげた例

bytesパッケージのBufferに実装されているメソッドを取ってくるコード。

```go
package main

import (
	"fmt"
	"go/types"
	"log"
	"sort"

	"golang.org/x/tools/go/loader"
)

func main() {
	c := loader.Config{
		TypeCheckFuncBodies: func(path string) bool { return false },
	}
	c.Import("bytes")

	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	info := prog.Package("bytes")
	ob := info.Pkg.Scope().Lookup("Buffer")

	if named, _ := ob.Type().(*types.Named); named != nil {
		var methods []*types.Func
		for i := 0; i < named.NumMethods(); i++ {
			methods = append(methods, named.Method(i))
		}

		sort.Slice(methods, func(i, j int) bool { return methods[i].Name() < methods[j].Name() })

		for _, m := range methods {
			fmt.Println(m)
		}
	}
}
```

結果

```go
func (*bytes.Buffer).Bytes() []byte
func (*bytes.Buffer).Cap() int
func (*bytes.Buffer).Grow(n int)
func (*bytes.Buffer).Len() int
func (*bytes.Buffer).Next(n int) []byte
func (*bytes.Buffer).Read(p []byte) (n int, err error)
func (*bytes.Buffer).ReadByte() (byte, error)
func (*bytes.Buffer).ReadBytes(delim byte) (line []byte, err error)
func (*bytes.Buffer).ReadFrom(r io.Reader) (n int64, err error)
func (*bytes.Buffer).ReadRune() (r rune, size int, err error)
func (*bytes.Buffer).ReadString(delim byte) (line string, err error)
func (*bytes.Buffer).Reset()
func (*bytes.Buffer).String() string
func (*bytes.Buffer).Truncate(n int)
func (*bytes.Buffer).UnreadByte() error
func (*bytes.Buffer).UnreadRune() error
func (*bytes.Buffer).Write(p []byte) (n int, err error)
func (*bytes.Buffer).WriteByte(c byte) error
func (*bytes.Buffer).WriteRune(r rune) (n int, err error)
func (*bytes.Buffer).WriteString(s string) (n int, err error)
func (*bytes.Buffer).WriteTo(w io.Writer) (n int64, err error)
func (*bytes.Buffer).empty() bool
func (*bytes.Buffer).grow(n int) int
func (*bytes.Buffer).readSlice(delim byte) (line []byte, err error)
func (*bytes.Buffer).tryGrowByReslice(n int) (int, bool)
```

ちなみに関数オブジェクト(types.Func)がExportedというメソッドを持っているので、非公開のものは除きたい場合にはそれを使えば良い。
