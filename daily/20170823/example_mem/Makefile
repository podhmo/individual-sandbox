default:

00:
	go run 00*
01:
	GOGCTRACE=1 go run 01*/main.go | sed "s@$$HOME@$$\HOME@g" > 01.output
02:
	GOGCTRACE=1 go run 02*/main.go | sed "s@$$HOME@$$\HOME@g" > 02.output
