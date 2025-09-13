# Go Standard Library Signature Extractor

## Overview

This tool is a Go program that scans the entire Go standard library and generates a new set of Go files containing only the public API signatures. It preserves all top-level constants (`const`), variables (`var`), and type definitions (`struct`, `interface`, etc.), but removes the bodies of all functions and methods.

This is useful for purposes like:
- Creating a lightweight, dependency-free mock of the standard library.
- API surface analysis and comparison between Go versions.
- Building tools that need to understand the standard library's API without needing the full implementation.

Function and method bodies are replaced with a `panic("not implemented")` statement, ensuring that the generated code is syntactically correct and compilable.

## Requirements

- Go 1.24 or newer.

## Usage

You can run the program directly using `go run`.

```bash
# Clone the repository (if applicable) and navigate to the directory
# ...

# Run the program
go run ./
```

The tool supports the following command-line flags:

-   `-output <directory>`: Specifies the directory where the generated files will be saved. The default is `./stdlib`.
-   `-debug`: Enables verbose debug logging to show detailed progress for each package.

### Examples

**Generate signatures into the default `stdlib` directory:**
```bash
go run main.go
```

**Generate signatures into a custom directory named `go-signatures` with debug logging:**
```bash
go run main.go -output go-signatures -debug
```

## Output

The tool will create a directory structure within the specified output directory that mirrors the standard library's package structure. For example, the signatures for the `fmt` package will be located at `<output_dir>/fmt.go`.

```
time=2025-09-13T23:22:44.324+09:00 level=INFO msg="Output directory prepared" path=stdlib
time=2025-09-13T23:22:44.324+09:00 level=INFO msg="Retrieving standard library package list..."
time=2025-09-13T23:22:44.453+09:00 level=INFO msg="Found 344 standard library packages"
time=2025-09-13T23:22:44.453+09:00 level=INFO msg="Processing package" package=archive/tar
time=2025-09-13T23:22:44.460+09:00 level=INFO msg="Processing package" package=archive/zip
time=2025-09-13T23:22:44.467+09:00 level=INFO msg="Processing package" package=bufio
time=2025-09-13T23:22:44.471+09:00 level=INFO msg="Processing package" package=bytes
time=2025-09-13T23:22:44.476+09:00 level=INFO msg="Processing package" package=cmp
time=2025-09-13T23:22:44.477+09:00 level=INFO msg="Processing package" package=compress/bzip2
time=2025-09-13T23:22:44.478+09:00 level=INFO msg="Processing package" package=compress/flate
time=2025-09-13T23:22:44.488+09:00 level=INFO msg="Processing package" package=compress/gzip
time=2025-09-13T23:22:44.492+09:00 level=INFO msg="Processing package" package=compress/lzw
time=2025-09-13T23:22:44.493+09:00 level=INFO msg="Processing package" package=compress/zlib
time=2025-09-13T23:22:44.495+09:00 level=INFO msg="Processing package" package=container/heap
time=2025-09-13T23:22:44.496+09:00 level=INFO msg="Processing package" package=container/list
time=2025-09-13T23:22:44.496+09:00 level=INFO msg="Processing package" package=container/ring
time=2025-09-13T23:22:44.497+09:00 level=INFO msg="Processing package" package=context
time=2025-09-13T23:22:44.499+09:00 level=INFO msg="Processing package" package=crypto
time=2025-09-13T23:22:44.500+09:00 level=INFO msg="Processing package" package=crypto/aes
time=2025-09-13T23:22:44.500+09:00 level=INFO msg="Processing package" package=crypto/cipher
time=2025-09-13T23:22:44.503+09:00 level=INFO msg="Processing package" package=crypto/des
time=2025-09-13T23:22:44.506+09:00 level=INFO msg="Processing package" package=crypto/dsa
time=2025-09-13T23:22:44.507+09:00 level=INFO msg="Processing package" package=crypto/ecdh
time=2025-09-13T23:22:44.508+09:00 level=INFO msg="Processing package" package=crypto/ecdsa
time=2025-09-13T23:22:44.509+09:00 level=INFO msg="Processing package" package=crypto/ed25519
time=2025-09-13T23:22:44.510+09:00 level=INFO msg="Processing package" package=crypto/elliptic
time=2025-09-13T23:22:44.513+09:00 level=INFO msg="Processing package" package=crypto/fips140
time=2025-09-13T23:22:44.513+09:00 level=INFO msg="Processing package" package=crypto/hkdf
time=2025-09-13T23:22:44.515+09:00 level=INFO msg="Processing package" package=crypto/hmac
time=2025-09-13T23:22:44.519+09:00 level=INFO msg="Processing package" package=crypto/md5
time=2025-09-13T23:22:44.520+09:00 level=INFO msg="Processing package" package=crypto/mlkem
time=2025-09-13T23:22:44.521+09:00 level=INFO msg="Processing package" package=crypto/pbkdf2
time=2025-09-13T23:22:44.522+09:00 level=INFO msg="Processing package" package=crypto/rand
time=2025-09-13T23:22:44.523+09:00 level=INFO msg="Processing package" package=crypto/rc4
time=2025-09-13T23:22:44.523+09:00 level=INFO msg="Processing package" package=crypto/rsa
time=2025-09-13T23:22:44.526+09:00 level=INFO msg="Processing package" package=crypto/sha1
time=2025-09-13T23:22:44.528+09:00 level=INFO msg="Processing package" package=crypto/sha256
time=2025-09-13T23:22:44.529+09:00 level=INFO msg="Processing package" package=crypto/sha3
time=2025-09-13T23:22:44.530+09:00 level=INFO msg="Processing package" package=crypto/sha512
time=2025-09-13T23:22:44.533+09:00 level=INFO msg="Processing package" package=crypto/subtle
time=2025-09-13T23:22:44.534+09:00 level=INFO msg="Processing package" package=crypto/tls
time=2025-09-13T23:22:44.558+09:00 level=INFO msg="Processing package" package=crypto/x509
time=2025-09-13T23:22:44.580+09:00 level=INFO msg="Processing package" package=crypto/x509/pkix
time=2025-09-13T23:22:44.580+09:00 level=INFO msg="Processing package" package=database/sql
time=2025-09-13T23:22:44.591+09:00 level=INFO msg="Processing package" package=database/sql/driver
time=2025-09-13T23:22:44.593+09:00 level=INFO msg="Processing package" package=debug/buildinfo
time=2025-09-13T23:22:44.594+09:00 level=INFO msg="Processing package" package=debug/dwarf
time=2025-09-13T23:22:44.600+09:00 level=INFO msg="Processing package" package=debug/elf
time=2025-09-13T23:22:44.627+09:00 level=INFO msg="Processing package" package=debug/gosym
time=2025-09-13T23:22:44.629+09:00 level=INFO msg="Processing package" package=debug/macho
time=2025-09-13T23:22:44.633+09:00 level=INFO msg="Processing package" package=debug/pe
time=2025-09-13T23:22:44.636+09:00 level=INFO msg="Processing package" package=debug/plan9obj
time=2025-09-13T23:22:44.637+09:00 level=INFO msg="Processing package" package=embed
time=2025-09-13T23:22:44.638+09:00 level=INFO msg="Processing package" package=encoding
time=2025-09-13T23:22:44.638+09:00 level=INFO msg="Processing package" package=encoding/ascii85
time=2025-09-13T23:22:44.640+09:00 level=INFO msg="Processing package" package=encoding/asn1
time=2025-09-13T23:22:44.648+09:00 level=INFO msg="Processing package" package=encoding/base32
time=2025-09-13T23:22:44.649+09:00 level=INFO msg="Processing package" package=encoding/base64
time=2025-09-13T23:22:44.651+09:00 level=INFO msg="Processing package" package=encoding/binary
time=2025-09-13T23:22:44.654+09:00 level=INFO msg="Processing package" package=encoding/csv
time=2025-09-13T23:22:44.657+09:00 level=INFO msg="Processing package" package=encoding/gob
time=2025-09-13T23:22:44.663+09:00 level=INFO msg="Processing package" package=encoding/hex
time=2025-09-13T23:22:44.664+09:00 level=INFO msg="Processing package" package=encoding/json
time=2025-09-13T23:22:44.678+09:00 level=INFO msg="Processing package" package=encoding/pem
time=2025-09-13T23:22:44.681+09:00 level=INFO msg="Processing package" package=encoding/xml
time=2025-09-13T23:22:44.700+09:00 level=INFO msg="Processing package" package=errors
time=2025-09-13T23:22:44.701+09:00 level=INFO msg="Processing package" package=expvar
time=2025-09-13T23:22:44.702+09:00 level=INFO msg="Processing package" package=flag
time=2025-09-13T23:22:44.705+09:00 level=INFO msg="Processing package" package=fmt
time=2025-09-13T23:22:44.710+09:00 level=INFO msg="Processing package" package=go/ast
time=2025-09-13T23:22:44.715+09:00 level=INFO msg="Processing package" package=go/build
time=2025-09-13T23:22:44.720+09:00 level=INFO msg="Processing package" package=go/build/constraint
time=2025-09-13T23:22:44.722+09:00 level=INFO msg="Processing package" package=go/constant
time=2025-09-13T23:22:44.725+09:00 level=INFO msg="Processing package" package=go/doc
time=2025-09-13T23:22:44.729+09:00 level=INFO msg="Processing package" package=go/doc/comment
time=2025-09-13T23:22:44.732+09:00 level=INFO msg="Processing package" package=go/format
time=2025-09-13T23:22:44.733+09:00 level=INFO msg="Processing package" package=go/importer
time=2025-09-13T23:22:44.734+09:00 level=INFO msg="Processing package" package=go/parser
time=2025-09-13T23:22:44.738+09:00 level=INFO msg="Processing package" package=go/printer
time=2025-09-13T23:22:44.742+09:00 level=INFO msg="Processing package" package=go/scanner
time=2025-09-13T23:22:44.745+09:00 level=INFO msg="Processing package" package=go/token
time=2025-09-13T23:22:44.748+09:00 level=INFO msg="Processing package" package=go/types
time=2025-09-13T23:22:44.776+09:00 level=INFO msg="Processing package" package=go/version
time=2025-09-13T23:22:44.777+09:00 level=INFO msg="Processing package" package=hash
time=2025-09-13T23:22:44.777+09:00 level=INFO msg="Processing package" package=hash/adler32
time=2025-09-13T23:22:44.778+09:00 level=INFO msg="Processing package" package=hash/crc32
time=2025-09-13T23:22:44.780+09:00 level=INFO msg="Processing package" package=hash/crc64
time=2025-09-13T23:22:44.781+09:00 level=INFO msg="Processing package" package=hash/fnv
time=2025-09-13T23:22:44.782+09:00 level=INFO msg="Processing package" package=hash/maphash
time=2025-09-13T23:22:44.784+09:00 level=INFO msg="Processing package" package=html
time=2025-09-13T23:22:44.794+09:00 level=INFO msg="Processing package" package=html/template
time=2025-09-13T23:22:44.807+09:00 level=INFO msg="Processing package" package=image
time=2025-09-13T23:22:44.810+09:00 level=INFO msg="Processing package" package=image/color
time=2025-09-13T23:22:44.811+09:00 level=INFO msg="Processing package" package=image/color/palette
time=2025-09-13T23:22:44.812+09:00 level=INFO msg="Processing package" package=image/draw
time=2025-09-13T23:22:44.815+09:00 level=INFO msg="Processing package" package=image/gif
time=2025-09-13T23:22:44.818+09:00 level=INFO msg="Processing package" package=image/jpeg
time=2025-09-13T23:22:44.824+09:00 level=INFO msg="Processing package" package=image/png
time=2025-09-13T23:22:44.826+09:00 level=INFO msg="Processing package" package=index/suffixarray
time=2025-09-13T23:22:44.828+09:00 level=INFO msg="Processing package" package=io
time=2025-09-13T23:22:44.831+09:00 level=INFO msg="Processing package" package=io/fs
time=2025-09-13T23:22:44.832+09:00 level=INFO msg="Processing package" package=io/ioutil
time=2025-09-13T23:22:44.833+09:00 level=INFO msg="Processing package" package=iter
time=2025-09-13T23:22:44.834+09:00 level=INFO msg="Processing package" package=log
time=2025-09-13T23:22:44.835+09:00 level=INFO msg="Processing package" package=log/slog
time=2025-09-13T23:22:44.842+09:00 level=INFO msg="Processing package" package=log/syslog
time=2025-09-13T23:22:44.843+09:00 level=INFO msg="Processing package" package=maps
time=2025-09-13T23:22:44.844+09:00 level=INFO msg="Processing package" package=math
time=2025-09-13T23:22:44.853+09:00 level=INFO msg="Processing package" package=math/big
time=2025-09-13T23:22:44.878+09:00 level=INFO msg="Processing package" package=math/bits
time=2025-09-13T23:22:44.880+09:00 level=INFO msg="Processing package" package=math/cmplx
time=2025-09-13T23:22:44.886+09:00 level=INFO msg="Processing package" package=math/rand
time=2025-09-13T23:22:44.891+09:00 level=INFO msg="Processing package" package=math/rand/v2
time=2025-09-13T23:22:44.895+09:00 level=INFO msg="Processing package" package=mime
time=2025-09-13T23:22:44.897+09:00 level=INFO msg="Processing package" package=mime/multipart
time=2025-09-13T23:22:44.901+09:00 level=INFO msg="Processing package" package=mime/quotedprintable
time=2025-09-13T23:22:44.901+09:00 level=INFO msg="Processing package" package=net
time=2025-09-13T23:22:44.949+09:00 level=INFO msg="Processing package" package=net/http
time=2025-09-13T23:22:44.978+09:00 level=INFO msg="Processing package" package=net/http/cgi
time=2025-09-13T23:22:44.980+09:00 level=INFO msg="Processing package" package=net/http/cookiejar
time=2025-09-13T23:22:44.985+09:00 level=INFO msg="Processing package" package=net/http/fcgi
time=2025-09-13T23:22:44.986+09:00 level=INFO msg="Processing package" package=net/http/httptest
time=2025-09-13T23:22:44.988+09:00 level=INFO msg="Processing package" package=net/http/httptrace
time=2025-09-13T23:22:44.988+09:00 level=INFO msg="Processing package" package=net/http/httputil
time=2025-09-13T23:22:44.993+09:00 level=INFO msg="Processing package" package=net/http/pprof
time=2025-09-13T23:22:44.994+09:00 level=INFO msg="Processing package" package=net/mail
time=2025-09-13T23:22:44.995+09:00 level=INFO msg="Processing package" package=net/netip
time=2025-09-13T23:22:44.999+09:00 level=INFO msg="Processing package" package=net/rpc
time=2025-09-13T23:22:45.002+09:00 level=INFO msg="Processing package" package=net/rpc/jsonrpc
time=2025-09-13T23:22:45.002+09:00 level=INFO msg="Processing package" package=net/smtp
time=2025-09-13T23:22:45.004+09:00 level=INFO msg="Processing package" package=net/textproto
time=2025-09-13T23:22:45.006+09:00 level=INFO msg="Processing package" package=net/url
time=2025-09-13T23:22:45.012+09:00 level=INFO msg="Processing package" package=os
time=2025-09-13T23:22:45.032+09:00 level=INFO msg="Processing package" package=os/exec
time=2025-09-13T23:22:45.036+09:00 level=INFO msg="Processing package" package=os/signal
time=2025-09-13T23:22:45.038+09:00 level=INFO msg="Processing package" package=os/user
time=2025-09-13T23:22:45.041+09:00 level=INFO msg="Processing package" package=path
time=2025-09-13T23:22:45.042+09:00 level=INFO msg="Processing package" package=path/filepath
time=2025-09-13T23:22:45.045+09:00 level=INFO msg="Processing package" package=plugin
time=2025-09-13T23:22:45.045+09:00 level=INFO msg="Processing package" package=reflect
time=2025-09-13T23:22:45.062+09:00 level=INFO msg="Processing package" package=regexp
time=2025-09-13T23:22:45.069+09:00 level=INFO msg="Processing package" package=regexp/syntax
time=2025-09-13T23:22:45.075+09:00 level=INFO msg="Processing package" package=runtime
time=2025-09-13T23:22:45.223+09:00 level=INFO msg="Processing package" package=runtime/cgo
time=2025-09-13T23:22:45.225+09:00 level=INFO msg="Processing package" package=runtime/coverage
time=2025-09-13T23:22:45.225+09:00 level=INFO msg="Processing package" package=runtime/debug
time=2025-09-13T23:22:45.226+09:00 level=INFO msg="Processing package" package=runtime/metrics
time=2025-09-13T23:22:45.228+09:00 level=INFO msg="Processing package" package=runtime/pprof
time=2025-09-13T23:22:45.235+09:00 level=INFO msg="Processing package" package=runtime/race
time=2025-09-13T23:22:45.236+09:00 level=INFO msg="Processing package" package=runtime/trace
time=2025-09-13T23:22:45.237+09:00 level=INFO msg="Processing package" package=slices
time=2025-09-13T23:22:45.241+09:00 level=INFO msg="Processing package" package=sort
time=2025-09-13T23:22:45.244+09:00 level=INFO msg="Processing package" package=strconv
time=2025-09-13T23:22:45.254+09:00 level=INFO msg="Processing package" package=strings
time=2025-09-13T23:22:45.259+09:00 level=INFO msg="Processing package" package=structs
time=2025-09-13T23:22:45.260+09:00 level=INFO msg="Processing package" package=sync
time=2025-09-13T23:22:45.264+09:00 level=INFO msg="Processing package" package=sync/atomic
time=2025-09-13T23:22:45.268+09:00 level=INFO msg="Processing package" package=syscall
time=2025-09-13T23:22:45.580+09:00 level=INFO msg="Processing package" package=testing
time=2025-09-13T23:22:45.587+09:00 level=INFO msg="Processing package" package=testing/fstest
time=2025-09-13T23:22:45.589+09:00 level=INFO msg="Processing package" package=testing/iotest
time=2025-09-13T23:22:45.590+09:00 level=INFO msg="Processing package" package=testing/quick
time=2025-09-13T23:22:45.591+09:00 level=INFO msg="Processing package" package=testing/slogtest
time=2025-09-13T23:22:45.592+09:00 level=INFO msg="Processing package" package=text/scanner
time=2025-09-13T23:22:45.595+09:00 level=INFO msg="Processing package" package=text/tabwriter
time=2025-09-13T23:22:45.596+09:00 level=INFO msg="Processing package" package=text/template
time=2025-09-13T23:22:45.605+09:00 level=INFO msg="Processing package" package=text/template/parse
time=2025-09-13T23:22:45.612+09:00 level=INFO msg="Processing package" package=time
time=2025-09-13T23:22:45.625+09:00 level=INFO msg="Processing package" package=time/tzdata
time=2025-09-13T23:22:45.642+09:00 level=INFO msg="Processing package" package=unicode
time=2025-09-13T23:22:45.681+09:00 level=INFO msg="Processing package" package=unicode/utf16
time=2025-09-13T23:22:45.681+09:00 level=INFO msg="Processing package" package=unicode/utf8
time=2025-09-13T23:22:45.683+09:00 level=INFO msg="Processing package" package=unique
time=2025-09-13T23:22:45.684+09:00 level=INFO msg="Processing package" package=unsafe
time=2025-09-13T23:22:45.684+09:00 level=INFO msg="Processing package" package=weak
time=2025-09-13T23:22:45.685+09:00 level=INFO msg="Signature generation complete."
```
