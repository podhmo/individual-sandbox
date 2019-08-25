template

```
1: # access undefined
2: 
3: {{xxx}} # <- accessing undefined variable, here.
```

## 3.7

Expected lineno. Because ` File "<template>", line 3, in top-level template code`.

```console
$ docker-compose run --rm python3.7 python render-with-undefined.py
Building python3.7
...
2.10.1
Traceback (most recent call last):
  File "render-with-undefined.py", line 9, in <module>
    print(jinja2.Template(t, undefined=jinja2.StrictUndefined).render())
  File "/usr/local/lib/python3.7/site-packages/jinja2/asyncsupport.py", line 76, in render
    return original_render(self, *args, **kwargs)
  File "/usr/local/lib/python3.7/site-packages/jinja2/environment.py", line 1008, in render
    return self.environment.handle_exception(exc_info, True)
  File "/usr/local/lib/python3.7/site-packages/jinja2/environment.py", line 780, in handle_exception
    reraise(exc_type, exc_value, tb)
  File "/usr/local/lib/python3.7/site-packages/jinja2/_compat.py", line 37, in reraise
    raise value.with_traceback(tb)
  File "<template>", line 3, in top-level template code
jinja2.exceptions.UndefinedError: 'xxx' is undefined
```

## 3.8

Unexpected lineno. Expected 3, but 1.  `File "<template>", line 1, in <module>`.

```console
$ docker-compose run --rm python3.8 python render-with-undefined.py
...
2.10.1
Traceback (most recent call last):
  File "render-with-undefined.py", line 9, in <module>
    print(jinja2.Template(t, undefined=jinja2.StrictUndefined).render())
  File "/usr/local/lib/python3.8/site-packages/jinja2/asyncsupport.py", line 76, in render
    return original_render(self, *args, **kwargs)
  File "/usr/local/lib/python3.8/site-packages/jinja2/environment.py", line 1008, in render
    return self.environment.handle_exception(exc_info, True)
  File "/usr/local/lib/python3.8/site-packages/jinja2/environment.py", line 780, in handle_exception
    reraise(exc_type, exc_value, tb)
  File "/usr/local/lib/python3.8/site-packages/jinja2/_compat.py", line 37, in reraise
    raise value.with_traceback(tb)
  File "<template>", line 1, in <module>
jinja2.exceptions.UndefinedError: 'xxx' is undefined
```
