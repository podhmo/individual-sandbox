define runT =
	cp $(1) public/`echo $(1) | sed 's@/@_@g'`
	go run $(1) | tee public/`echo $(1) | sed 's@/@_@g; s@\.go$$@.output.txt@'`
endef
define cpT =
	cp $(1) public/`echo $(1) | sed 's@/@_@g'`
endef

default: clean
	mkdir -p public
	$(call cpT,Makefile)
	$(call runT,00/main.go)
	$(call runT,01/main.go)
	$(call runT,02/main.go)
	$(call runT,03/main.go)
	$(call runT,04/main.go)
	$(call runT,05/main.go)
	$(call runT,06/main.go)
	$(call runT,07/main.go)
	$(call runT,08/main.go)

clean:
	rm -rf public
.PHONY: clean
