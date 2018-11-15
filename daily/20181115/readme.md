## python starletteの中をのぞく

- black使っているのか
- starletteとstarlette[full]は別物
- testsを外に出すものも見かける様になった
- ?toplevelのディレクトリは何なんだろう？ -> applications?
- uvicornに依存しているのではなくASGIなのでuvicornで実行するという感じ

```python
from starlette.applications import Starlette
from starlette.responses import JSONResponse
import uvicorn

app = Starlette()

@app.route('/')
async def homepage(request):
    return JSONResponse({'hello': 'world'})

if __name__ == '__main__':
    uvicorn.run(app, host='0.0.0.0', port=8000)
```

interface

```python
Scope = typing.MutableMapping[str, typing.Any]
Message = typing.MutableMapping[str, typing.Any]

class ASGIInstance:
    class Receive:
        def __call__(self) -> typing.Awaitable[Message]:
            ...

    class Send:
        def __call__(self, message: Message) -> typing.Awaitable[None]:
            ...

    def __call__(self, receive: Receive, send: Send) -> typing.Awaitable[None]:
        ...

class ASGIApp:
    def __call__(self, scope: Scope) -> ASGIInstance:
        ...
```

### starletteのasgiappの意味は？

```python
class Starlette:
    def __init__(self, debug: bool = False, template_directory: str = None) -> None:
        self._debug = debug
        self.router = Router()
        self.lifespan_handler = LifespanHandler()
        self.exception_middleware = ExceptionMiddleware(self.router, debug=debug)
        self.error_middleware = ServerErrorMiddleware(
            self.exception_middleware, debug=debug
        # )
        self.schema_generator = None  # type: typing.Optional[BaseSchemaGenerator]
        self.template_env = self.load_template_env(template_directory)


    def __call__(self, scope: Scope) -> ASGIInstance:
        scope["app"] = self
        if scope["type"] == "lifespan":
            return self.lifespan_handler(scope)
        return self.error_middleware(scope)
```

それ以外はappというよりregistryというか便利オブジェクトという感じか

```python
class LifespanHandler:
    async def run_lifespan(self, receive: Receive, send: Send) -> None:
        message = await receive()
        assert message["type"] == "lifespan.startup"
        await self.run_startup()
        await send({"type": "lifespan.startup.complete"})
        message = await receive()
        assert message["type"] == "lifespan.shutdown"
        await self.run_shutdown()
        await send({"type": "lifespan.shutdown.complete"})
```

## go ast メソッドと関数見分けるのどうするんだっけ？

`*ast.FuncDecl` が `Recv` を持っていたらMethod

### メソッドを名前で取ることできたっけ？

無理そう。scopeに保存されない。トップレベルの関数以外。

```go
func (p *parser) parseFuncDecl() *ast.FuncDecl {
...
	if recv == nil {
		// Go spec: The scope of an identifier denoting a constant, type,
		// variable, or function (but not method) declared at top level
		// (outside any function) is the package block.
		//
		// init() functions cannot be referred to and there may
		// be more than one - don't put them in the pkgScope
		if ident.Name != "init" {
			p.declare(decl, nil, p.pkgScope, ast.Fun, ident)
		}
	}

	return decl
}
```

## go 久しぶりにapi serverを

- net/http/httptest
- テスト結果をどう書くのが良いんだろう？

## gocode

なんか動いていないんですけど

```lisp
(setq debug-on-error t)
```

```
Debugger entered--Lisp error: (wrong-type-argument stringp nil)
  propertize(nil meta "2018/11/15 01:20:31 rpc: can't find service Server.AutoComplete  " package nil)
  (let ((candidate (split-string str ",,"))) (propertize (nth 1 candidate) 'meta (company-go--format-meta candidate) 'package (nth 3 candidate)))
  (lambda (str) (let ((candidate (split-string str ",,"))) (propertize (nth 1 candidate) 'meta (company-go--format-meta candidate) 'package (nth 3 candidate))))("2018/11/15 01:20:31 rpc: can't find service Server.AutoComplete")
  mapcar((lambda (str) (let ((candidate (split-string str ",,"))) (propertize (nth 1 candidate) 'meta (company-go--format-meta candidate) 'package (nth 3 candidate)))) ("2018/11/15 01:20:31 rpc: can't find service Server.AutoComplete"))
  company-go--get-candidates(("2018/11/15 01:20:31 rpc: can't find service Server.AutoComplete"))
  (let ((candidates (company-go--get-candidates (split-string (company-go--invoke-autocomplete) "\n" t)))) (if (equal candidates '("PANIC")) (error "GOCODE PANIC: Please check your code by \"go build\"") candidates))
  company-go--candidates()
  (cond ((eql command 'interactive) (company-begin-backend 'company-go)) ((eql command 'prefix) (and (derived-mode-p 'go-mode) (not (company-in-string-or-comment)) (not (company-go--in-num-literal-p)) (or (company-go--prefix) 'stop))) ((eql command 'candidates) (company-go--candidates)) ((eql command 'meta) (company-go--syntax-highlight (get-text-property 0 'meta arg))) ((eql command 'annotation) (if company-go-show-annotation (progn (company-go--extract-annotation (get-text-property 0 'meta arg))))) ((eql command 'location) (company-go--location arg)) ((eql command 'doc-buffer) (company-go--godoc-as-buffer arg)) ((eql command 'sorted) t) ((eql command 'post-completion) (if (and company-go-insert-arguments (not (char-equal 40 (following-char)))) (progn (company-go--insert-arguments (get-text-property 0 'meta arg))))))
  company-go(candidates #("mux" 0 3 (fontified t)))
  apply(company-go (candidates #("mux" 0 3 (fontified t))))
  company-call-backend-raw(candidates #("mux" 0 3 (fontified t)))
  apply(company-call-backend-raw (candidates #("mux" 0 3 (fontified t))))
  company--force-sync(company-call-backend-raw (candidates #("mux" 0 3 (fontified t))) company-go)
  company-call-backend(candidates #("mux" 0 3 (fontified t)))
  company--fetch-candidates(#("mux" 0 3 (fontified t)))
  company-calculate-candidates(#("mux" 0 3 (fontified t)))
  company--begin-new()
  company--perform()
  company-auto-begin()
  company-manual-begin()
  company-complete()
  funcall-interactively(company-complete)
  #<subr call-interactively>(company-complete nil nil)
  apply(#<subr call-interactively> company-complete (nil nil))
  call-interactively@ido-cr+-record-current-command(#<subr call-interactively> company-complete nil nil)
  apply(call-interactively@ido-cr+-record-current-command #<subr call-interactively> (company-complete nil nil))
  call-interactively(company-complete nil nil)
  command-execute(company-complete)
```

```
(1 475 "gocode" nil #<buffer  *temp*> nil "-f=csv-with-package" "autocomplete" "/home/nao/venvs/my/individual-sandbox/daily/20181115/example_goapi/internal/server.go" "c457")
```

```
$ cat example_goapi/internal/server.go | gocode -debug -f=csv-with-package autocomplete `pwd`/example_goapi/internal/server.go "c430"
2018/11/15 01:33:47 rpc: can't find service Server.AutoComplete
$ ps aux | grep gocode
nao      19159  0.0  0.0   9212  2560 pts/2    S+   01:33   0:00 grep --color=auto gocode
nao      25258  0.0  0.0 699548  8648 ?        Sl   00:41   0:01 /home/nao/go/bin/gocode -s -sock unix -addr 127.0.0.1:37373
```

## python prompt toolkit
