# python subprocessで出力をmixして出す方法

```python
import subprocess

subprocess.run(cmd, shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
```
