default: golang.tokyo.csv gocon.csv
	$(MAKE) create-list

setup:
	go get -v github.com/PuerkitoBio/goquery
	go get -v gopkg.in/alecthomas/kingpin.v2
	go get -v github.com/ChimeraCoder/anaconda

create-list:
	go run 01list/main.go -c ~/.config/egoist/config.json --username podhmo --listname html5app html5app.csv

golang.tokyo.csv:
	go run 00extract/main.go "https://golangtokyo.connpass.com/event/82723/participation/#participants" | tee $@

gocon.csv:
	go run 00extract/main.go "https://gocon.connpass.com/event/82515/participation/#participants" | tee $@

html5app.csv:
	go run 00extract/main.go "https://html5app-conf.connpass.com/event/87730/participation/#participants" | tee $@

