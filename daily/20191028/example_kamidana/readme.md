# kamidana

直っていたけれど。バグっていた。

## hmm

差分

```diff
--- /tmp/before	2019-10-29 22:57:46.624959743 +0900
+++ /tmp/after	2019-10-29 22:57:57.301768973 +0900
@@ -19,6 +19,8 @@
     raise value.with_traceback(tb)
   File "main.j2", line 2, in top-level template code
     {% include "./zero.div.html.j2" %}
-  File "VENV/individual-sandbox/daily/20191028/example_kamidana/00/zero.div.html.j2", line 2, in top-level template code
-    <pre>1 / 0 = {{ 1 / 0 }}</pre>
+  File "VENV/lib/python3.7/site-packages/jinja2/environment.py", line 1005, in render
+    return concat(self.root_render_func(self.new_context(vars)))
+  File "main.j2", line 12, in root
+  File "VENV/individual-sandbox/daily/20191028/example_kamidana/00/zero.div.html.j2", line 11, in root
 ZeroDivisionError: division by zero
```

aggregate

```diff
$ diff -u /tmp/before2 /tmp/after2
--- /tmp/before2        2019-10-29 23:01:37.891713457 +0900
+++ /tmp/after2 2019-10-29 23:01:10.381274385 +0900
@@ -8,4 +8,6 @@
 @@ python <FrameSummary file VENV/lib/python3.7/site-packages/jinja2/environment.py, line 780 in handle_exception>
 @@ python <FrameSummary file VENV/lib/python3.7/site-packages/jinja2/_compat.py, line 37 in reraise>
 @@ jinja2 <FrameSummary file main.j2, line 2 in top-level template code>
-@@ jinja2 <FrameSummary file VENV/individual-sandbox/daily/20191028/example_kamidana/00/zero.div.html.j2, line 2 in top-level template code>
+@@ python <FrameSummary file VENV/lib/python3.7/site-packages/jinja2/environment.py, line 1005 in render>
+@@ python <FrameSummary file main.j2, line 12 in root>
+@@ python <FrameSummary file VENV/individual-sandbox/daily/20191028/example_kamidana/00/zero.div.html.j2, line 11 in root>
```
