```bash
$ make
zenmai template.yaml --data "data.yaml#/app/local" | nejimaki --position=dist
 INFO	  nejimaki	emit:dist/I.local.yaml
 INFO	  nejimaki	emit:dist/J.local.yaml
 INFO	  nejimaki	emit:dist/K.local.yaml
zenmai template.yaml --data "data.yaml#/app/dev" | nejimaki --position=dist
 INFO	  nejimaki	emit:dist/I.dev.yaml
 INFO	  nejimaki	emit:dist/J.dev.yaml
 INFO	  nejimaki	emit:dist/K.dev.yaml
zenmai template.yaml --data "data.yaml#/app/production" | nejimaki --position=dist
 INFO	  nejimaki	emit:dist/I.production.yaml
 INFO	  nejimaki	emit:dist/J.production.yaml
 INFO	  nejimaki	emit:dist/K.production.yaml
```
