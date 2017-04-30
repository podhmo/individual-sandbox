default:
	jsonknife deref --ref "#/production" --src ../example_config/config.yaml > production.yaml
	jsonknife cut --ref "#/production" --src ../example_config/config.yaml | sed 's@#/production@./production.yaml@g'> config.yaml
	zenmai config.yaml --select '#/testing' > testing.yaml

