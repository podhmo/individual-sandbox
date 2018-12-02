```console
$ mage -keep -debug ls
DEBUG: 20:29:20.926658 getting all non-mage files in .
DEBUG: 20:29:20.941564 found non-mage files
DEBUG: 20:29:20.941577 getting all files plus mage files
DEBUG: 20:29:20.976741 time to scan for Magefiles: 50.096627ms
DEBUG: 20:29:20.976752 found magefiles: magefile.go
DEBUG: 20:29:20.984057 output exe is  $HOME/.magefile/5eb48aa7125cbb7d7a85a15ad4af2e980b1a3107
DEBUG: 20:29:20.984081 running go env GOCACHE
DEBUG: 20:29:20.994066 build cache exists, will ignore any compiled binary
DEBUG: 20:29:20.994080 parsing files
DEBUG: 20:29:20.994263 found target Build
DEBUG: 20:29:20.994276 found target Clean
DEBUG: 20:29:20.994282 found target Install
DEBUG: 20:29:20.994289 found target InstallDeps
DEBUG: 20:29:20.994298 found target Ls
DEBUG: 20:29:20.994307 time parse Magefiles: 221.69µs
DEBUG: 20:29:20.994315 Creating mainfile at mage_output_file.go
DEBUG: 20:29:20.994361 writing new file at mage_output_file.go
DEBUG: 20:29:20.994977 compiling to $HOME/.magefile/5eb48aa7125cbb7d7a85a15ad4af2e980b1a3107
DEBUG: 20:29:20.994983 compiling using gocmd: go
DEBUG: 20:29:20.994989 running go version
DEBUG: 20:29:21.003672 go version go1.11.1 linux/amd64

DEBUG: 20:29:21.003686 running go env
DEBUG: 20:29:21.048267 GOARCH="amd64"
GOBIN=""
GOCACHE="$HOME/.cache/go-build"
GOEXE=""
GOFLAGS=""
GOHOSTARCH="amd64"
GOHOSTOS="linux"
GOOS="linux"
GOPATH="$GOPATH"
GOPROXY=""
GORACE=""
GOROOT="/usr/lib/go"
GOTMPDIR=""
GOTOOLDIR="/usr/lib/go/pkg/tool/linux_amd64"
GCCGO="gccgo"
CC="gcc"
CXX="g++"
CGO_ENABLED="1"
GOMOD=""
CGO_CFLAGS="-g -O2"
CGO_CPPFLAGS=""
CGO_CXXFLAGS="-g -O2"
CGO_FFLAGS="-g -O2"
CGO_LDFLAGS="-g -O2"
PKG_CONFIG="pkg-config"
GOGCCFLAGS="-fPIC -m64 -pthread -fmessage-length=0 -fdebug-prefix-map=/tmp/go-build029282804=/tmp/go-build -gno-record-gcc-switches"

DEBUG: 20:29:21.048310 running go -tag=mage build -o $HOME/.magefile/5eb48aa7125cbb7d7a85a15ad4af2e980b1a3107 magefile.go mage_output_file.go
DEBUG: 20:29:21.105828 time to compile Magefile: 57.463746ms
DEBUG: 20:29:21.105846 keeping mainfile
DEBUG: 20:29:21.105854 running binary $HOME/.magefile/5eb48aa7125cbb7d7a85a15ad4af2e980b1a3107
DEBUG: 20:29:21.105882 running magefile with mage vars:
MAGEFILE_DEBUG=1
call ls command
magefile.go  mage_output_file.go  Makefile
```
