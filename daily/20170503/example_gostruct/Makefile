gen:
	mkdir -p output
	jsonknife deref --src swagger.yaml --ref '#/definitions/conf' | jsonknife samplevalue > config.json
	python gen.py --ref="#/definitions/conf" --package=main swagger.yaml > output/models.go
	goimports -w output/models.go

play:
	cat config.json | go run handmade/*.go

play2:
	cat config.json | go run output/*.go
