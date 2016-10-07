# go mockで現在のpackageで定義したtype aliasなどを使う

自動化はできていないけれど。以下の様にするとどうにか動かせる。
importのところだけが重要

foo.go
```go
package foo
type S int
```

mock_foo/mock_foo.go
```go
package mock_foo

import (
	. "github.com/podhmo/foo"
	gomock "github.com/golang/mock/gomock"
)
```

foo_test.go
```go
package foo_test

import (
	"testing"

	"github.com/golang/mock/gomock"
	. "github.com/podhmo/foo"
	mock "github.com/podhmo/foo/mock_foo"
)
```
