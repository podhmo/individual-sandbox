#!/usr/local/bin/python3
import cgitb

cgitb.enable(display=0, logdir="logdir")
# http :8080/cgi-bin/hello.py
print("""Content-Type: text/html; charset=utf-8

<html><body>
<p>
こんにちは　日本語！
</p>
<div id="box"></div>
<script>
fetch("app.py")
  .then((res) => res.json()).then((data) => {document.querySelector("#box").innerHTML = `<pre>${JSON.stringify(data)}</pre>`; })
  .catch((err) => { alert(err); })
</script>
</body></html>
""")
