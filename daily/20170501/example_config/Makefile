default:
	dictknife concat config.json -f yaml | sed 's@_extends: production@$$inherit: "#/production"@g;s@_extends: staging@$$inherit: "#/staging"@g' > config.yaml
	zenmai config.yaml --select '#/development' > dev.yaml
