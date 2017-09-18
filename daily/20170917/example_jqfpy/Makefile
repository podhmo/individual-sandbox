Gopkg.json: Gopkg.toml
	dictknife concat $< --dst $@

Gopkg.toml:
	wget https://raw.githubusercontent.com/golang/dep/master/Gopkg.toml

clean:
	rm Gopkg*
