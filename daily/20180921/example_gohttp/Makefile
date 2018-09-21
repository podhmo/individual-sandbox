00:
	go run 00/main.go
.PHONY: 00

00client:
	http http://localhost:4444/

bench:
	echo 'GET http://localhost:4444/' | vegeta attack -rate 10000 -duration 5s | tee /tmp/xxx | vegeta report
