```bash
$ make
zenmai one.template.yaml --data "./data.yaml#/local" --data "./data.yaml#/services/one" > one.local.yaml
zenmai one.template.yaml --data "./data.yaml#/dev" --data "./data.yaml#/services/one" > one.dev.yaml
zenmai one.template.yaml --data "./data.yaml#/production" --data "./data.yaml#/services/one" > one.production.yaml
```
