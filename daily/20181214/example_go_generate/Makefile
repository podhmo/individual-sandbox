init:
	rm -rf g1 g2 vendor
	mkdir -p g1 g2 vendor/g3
	echo -e 'package g1\n//go:generate echo one' > g1/file1.go
	echo -e 'package g1\n//go:generate echo two' > g1/file2.go
	echo -e 'package g2\n//go:generate echo three' > g2/file3.go
	echo -e 'package g3\n//go:generate echo four' > vendor/g3/file4.go
	tree
	go generate ./...
