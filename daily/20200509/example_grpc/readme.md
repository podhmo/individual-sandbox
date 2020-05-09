```console
$ EX_SRC_DIR="$(go env GOPATH)/pkg/mod/google.golang.org/grpc@v1.29.1/examples"
$ cp -R "$EX_SRC_DIR/helloworld" "$MY_EXAMPLES"
$ perl -pi -e 's|google.golang.org/grpc/examples|m|g' helloworld/greeter_{client,server}/main.go
```
