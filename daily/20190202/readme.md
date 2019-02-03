## python ipython

ipythonのpromptを変えたいかも？

https://ipython.readthedocs.io/en/stable/config/details.html

とりあえず以下にstartupのscriptを用意する

```
$ cat ~/.ipython/profile_default/startup/README
This is the IPython startup directory

.py and .ipy files in this directory will be run *prior* to any code or files specified
via the exec_lines or exec_files configurables whenever you load this profile.

Files will be run in lexicographical order, so you can control the execution order of files
with a prefix, e.g.::

    00-first.py
    50-middle.py
    99-last.ipy
$ edit ~/.ipython/profile_default/startup/00-prompt.py
```

00-prompt.py

```python
import os


def apply_legacy_prompt():
    import IPython.terminal.prompts as prompts
    from IPython import get_ipython

    class Prompts(prompts.Prompts):
        def in_prompt_tokens(self, cli=None):
            return [(prompts.Token.Prompt, ">>> ")]

        def out_prompt_tokens(self, cli=None):
            return []

    ip = get_ipython()
    ip.prompts = Prompts(ip)


if os.environ.get("LEGACY") is not None:
    apply_legacy_prompt()
```

