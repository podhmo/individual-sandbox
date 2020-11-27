export ADDR := :33333

00:
	go run $(shell echo $@*/)cmd/foo-api/main.go
x00:
	echo '{"name": "foo"}' | http --json POST $(ADDR)/Hello
	echo '{"name": "foo"}' | http --json POST $(ADDR)/Hello short==true
x01:
	echo '10' | http --json POST $(ADDR)/IsEven
	echo '9' | http --json POST $(ADDR)/IsEven
	echo 'foo' | http --json POST $(ADDR)/IsEven
