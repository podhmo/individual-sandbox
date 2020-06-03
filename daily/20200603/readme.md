## egoist rpcを動かす

## go gofmtをrpc化

goplsの中を覗きたい。	"go/format"　？

hmm

- DidChangeTextDocument Notification
- Document Range Formatting Request
- Document Formatting Request

onTypeFormating, 

### install

```console
$ go get golang.org/x/tools/gopls@latest
```


> ### Edit assistance

> These features suggest or apply edits to the code for the user, including refactoring features, for which there are many potential use cases.
> Refactoring is one of the places where Go tools could potentially be very strong, but have not been so far, and thus there is huge potential for improvements in the developer experience.
> There is not yet a clear understanding of the kinds of refactoring people need or how they should express them however, and there are weaknesses in the LSP protocol around this.
> This means it may be  much more of a research project.

---
Format   | Fix the formatting of the file
-------- | ---
Requires | AST of current file
LSP      | [`textDocument/formatting`]
|        | [`textDocument/rangeFormatting`]
|        | [`textDocument/onTypeFormatting`]
Previous | [gofmt], [goimports], [goreturns]
|        | It will use the standard format package. <br/> Current limitations are that it does not work on malformed code. It may need some very careful changes to the formatter to allow for formatting an invalid AST or changes to force the AST to a valid mode. These changes would improve range and file mode as well, but are basically vital to onTypeFormatting
