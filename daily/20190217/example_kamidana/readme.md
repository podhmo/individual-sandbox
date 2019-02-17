pythonのコードでミニマムに表すと以下の様な形になる。

```python
import sys
import jinja2

env = jinja2.Environment(loader=jinja2.FileSystemLoader(os.getcwd()))
template = env.get_template(sys.argv[1])
print(template.render())
```
