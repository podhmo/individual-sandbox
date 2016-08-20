reflection -> type assertion

```diff
--- 01masking.go	2016-08-20 15:38:07.000000000 +0900
+++ 02masking.go	2016-08-20 15:37:58.000000000 +0900
@@ -4,7 +4,6 @@
 	"encoding/json"
 	"fmt"
 	"log"
-	"reflect"
 	"regexp"
 )
 
@@ -16,11 +15,12 @@
 type Maskable interface {
 	Mask() interface{}
 }
+type I interface {
+}
 
-func Mask(o interface{}) interface{} {
-	v := reflect.ValueOf(o)
-	if t, ok := v.Interface().(Maskable); ok {
-		return t.Mask()
+func Mask(o I) I {
+	if o, ok := o.(Maskable); ok {
+		return o.Mask()
 	}
 	return o
 }
```

type assertion (if) -> type assertion (switch)

```diff
--- 02masking.go	2016-08-20 15:37:58.000000000 +0900
+++ 03masking.go	2016-08-20 15:48:23.000000000 +0900
@@ -19,10 +19,12 @@
 }
 
 func Mask(o I) I {
-	if o, ok := o.(Maskable); ok {
+	switch o := o.(type) {
+	case Maskable:
 		return o.Mask()
+	default:
+		return o
 	}
-	return o
 }
 
 func (u User) Mask() interface{} {
```
