default: 00 01 02 03 04

# PKG := github.com/pkg/errors
# PKG := io
PKG := golang.org/x/tools/go/types/typeutil


00:
	gopackages -json -mode files ${PKG} | tee 00files.json
01:
	gopackages -json -mode imports ${PKG} | tee 01imports.json
02:
	gopackages -json -mode types ${PKG} | tee 02types.json
03:
	gopackages -json -mode syntax ${PKG} | tee 03syntax.json
04:
	gopackages -json -mode allsyntax ${PKG} | tee 04allsyntax.json

setup:
	vgo get -v golang.org/x/tools/go/packages/gopackages

