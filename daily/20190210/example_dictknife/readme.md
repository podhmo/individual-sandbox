```
python 04*.py --src src/main.yaml
DEBUG:jsonknife.resolver:load file[1]: 'src/main.yaml' (where='*root*')
DEBUG:dictknife.swaggerknife.migration:prepare (where=VENV/individual-sandbox/daily/20190210/example_dictknife)
DEBUG:jsonknife.resolver:load file[2]: 'person.yaml' (where='VENV/individual-sandbox/daily/20190210/example_dictknife/src/main.yaml')
DEBUG:jsonknife.resolver:load file[3]: 'primitive.yaml' (where='VENV/individual-sandbox/daily/20190210/example_dictknife/src/person.yaml')
DEBUG:jsonknife.resolver:load file[2]: 'db.yaml' (where='VENV/individual-sandbox/daily/20190210/example_dictknife/src/main.yaml')
INFO:dictknife.swaggerknife.migration:migrate dry run and diff
--- before:src/person.yaml
+++  after:src/person.yaml
@@ -7,6 +7,9 @@
           "$ref": "primitive.yaml#/definitions/name"
         },
         "age": {
+          "type": "integer"
+        },
+        "value": {
           "type": "integer"
         }
       }
--- before:src/primitive.yaml
+++  after:src/primitive.yaml
@@ -1,7 +1,8 @@
 {
   "definitions": {
     "name": {
-      "type": "string"
+      "type": "string",
+      "description": "name of something"
     }
   }
 }
```
