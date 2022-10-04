output := /tmp/x

run: $(output)
	$(output) > output.txt
	readelf -S $(output) > output.readelf.section-headers.txt
	readelf -l $(output) > output.readelf.program-headers.txt
	readelf -h $(output) > output.readelf.file-header.txt
	readelf -s $(output) > output.readelf.symbols.txt
	objdump -s -j .go.buildinfo $(output) > output.objdump.go.buildinfo.txt
.PHONY: run	

$(output): main.go
	go build -o $@