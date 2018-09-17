字句解析なのでLexerみたいな名前が関係していそう。あとmagic commandsという名前にも触れそう。検索すると以下のページが見つかる（正確には以下のページの古いバージョン）

- [New IPython Console Lexer — IPython 6.5.0 documentation](https://ipython.readthedocs.io/en/stable/development/lexer.html "New IPython Console Lexer — IPython 6.5.0 documentation")

ちょこっとだけ覗いてみると、探していたもののような感じ。IPythonLexerを覗ければ良い。

> The IPython console lexer has been rewritten and now supports tracebacks and customized input/output prompts. An entire suite of lexers is now available at IPython.lib.lexers. These include:
> IPythonLexer & IPython3Lexer
>    Lexers for pure IPython (python + magic/shell commands)

IPythonLexerを探してみる。

```console
$ grep -rl IPythonLexer $(pyinspect resolve IPython)  | grep -v pyc
VENV/lib/python3.7/site-packages/IPython/lib/lexers.py
VENV/lib/python3.7/site-packages/IPython/lib/tests/test_lexers.py
```

それっぽいものが見つかる。メソッドなどを覗いてみる。pygmentsのlexerを流用している模様。

```console
$ pyinspect inspect IPython.lib.lexers:IPythonLexer
pygments.lexer.IPython <- pygments.lexers.python.PythonLexer <- pygments.lexer.RegexLexer <- pygments.lexer.Lexer <- builtins.object

pygments.lexers.python.PythonLexer <- pygments.lexer.RegexLexer <- pygments.lexer.Lexer <- builtins.object
    [static method, OVERRIDE] analyse_text(text)
    [method] innerstring_rules(ttype)

pygments.lexer.RegexLexer <- pygments.lexer.Lexer <- builtins.object
    [method, OVERRIDE] get_tokens_unprocessed(self, text, stack=('root',))

pygments.lexer.Lexer <- builtins.object
    [method, OVERRIDE] __init__(self, **options)
        [method] add_filter(self, filter_, **options)
    [method, OVERRIDE] __repr__(self)
    [static method] analyse_text(text)
    [method] get_tokens(self, text, unfiltered=False)
        [method] get_tokens_unprocessed(self, text)
```

生成のされ方を見ると以下の様になっている。元々持っているtokensにさらに`ipython_tokens`という変数に持っている値を追加してlexerを作っている。

```python
def build_ipy_lexer(python3):
    """Builds IPython lexers depending on the value of `python3`.

    The lexer inherits from an appropriate Python lexer and then adds
    information about IPython specific keywords (i.e. magic commands,
    shell commands, etc.)

    Parameters
    ----------
    python3 : bool
        If `True`, then build an IPython lexer from a Python 3 lexer.

    """
    # It would be nice to have a single IPython lexer class which takes
    # a boolean `python3`.  But since there are two Python lexer classes,
    # we will also have two IPython lexer classes.
    if python3:
        PyLexer = Python3Lexer
        name = 'IPython3'
        aliases = ['ipython3']
        doc = """IPython3 Lexer"""
    else:
        PyLexer = PythonLexer
        name = 'IPython'
        aliases = ['ipython2', 'ipython']
        doc = """IPython Lexer"""

    tokens = PyLexer.tokens.copy()
    tokens['root'] = ipython_tokens + tokens['root']

    attrs = {'name': name, 'aliases': aliases, 'filenames': [],
             '__doc__': doc, 'tokens': tokens}

    return type(name, (PyLexer,), attrs)


IPython3Lexer = build_ipy_lexer(python3=True)
IPythonLexer = build_ipy_lexer(python3=False)
```

ここでipython_tokensの定義はこういう感じ。

```python
ipython_tokens = [
  (r"(?s)(\s*)(%%)(\w+)(.*)", bygroups(Text, Operator, Keyword, Text)),
  (r'(?s)(^\s*)(%%!)([^\n]*\n)(.*)', bygroups(Text, Operator, Text, using(BashLexer))),
  (r"(%%?)(\w+)(\?\??)$",  bygroups(Operator, Keyword, Operator)),
  (r"\b(\?\??)(\s*)$",  bygroups(Operator, Text)),
  (r'(%)(sx|sc|system)(.*)(\n)', bygroups(Operator, Keyword,
                                       using(BashLexer), Text)),
  (r'(%)(\w+)(.*\n)', bygroups(Operator, Keyword, Text)),
  (r'^(!!)(.+)(\n)', bygroups(Operator, using(BashLexer), Text)),
  (r'(!)(?!=)(.+)(\n)', bygroups(Operator, using(BashLexer), Text)),
  (r'^(\s*)(\?\??)(\s*%{0,2}[\w\.\*]*)', bygroups(Text, Operator, Text)),
  (r'(\s*%{0,2}[\w\.\*]*)(\?\??)(\s*)$', bygroups(Text, Operator, Text)),
]
```

おそらくこのあたりがmagic command関係。

```
  (r"(%%?)(\w+)(\?\??)$",  bygroups(Operator, Keyword, Operator)),
  (r'(%)(\w+)(.*\n)', bygroups(Operator, Keyword, Text)),
```

ちなみに

- `% foo` のほうはline magic。こちらは行単位のコマンド
- `%% foo` のほうはcell magic。こちらはcell単位のコマンド(1つのcellは複数の行を持てる)

### 脇道: pygmentsのLexerの使いかた

pygmentsはsyntax highlightなどに使われるライブラリなのだけれど。このlexerの使いかたを調べてみたくなる。実際に使ってみる。

`Lexer.get_tokens()` を呼ぶと良い感じにtokenが取り出せるようになるらしい。

```python
from pygments.lexers import BashLexer, PythonLexer, Python3Lexer
from pygments.lexers import Python3Lexer

code = """
def hello(name: str) -> None:
    print(f"{name}: hello")
"""

lexer = Python3Lexer()
for token in lexer.get_tokens(code.strip()):
    print(token)

# (Token.Keyword, 'def')
# (Token.Text, ' ')
# (Token.Name.Function, 'hello')
# (Token.Punctuation, '(')
# (Token.Name, 'name')
# (Token.Punctuation, ':')
# (Token.Text, ' ')
# (Token.Name.Builtin, 'str')
# (Token.Punctuation, ')')
# (Token.Text, ' ')
# (Token.Operator, '-')
# (Token.Operator, '>')
# (Token.Text, ' ')
# (Token.Keyword.Constant, 'None')
# (Token.Punctuation, ':')
# (Token.Text, '\n')
# (Token.Text, '    ')
# (Token.Name.Builtin, 'print')
# (Token.Punctuation, '(')
# (Token.Name, 'f')
# (Token.Literal.String.Double, '"')
# (Token.Literal.String.Interpol, '{name}')
# (Token.Literal.String.Double, ': hello')
# (Token.Literal.String.Double, '"')
# (Token.Punctuation, ')')
# (Token.Text, '\n')
```

なので当然、ipython用のlexerを使えば `%matplotlib inline` は取り出せる。

```python
from IPython.lib.lexers import IPython3Lexer

code = """
%matplotlib inline
"""
lexer = IPython3Lexer()
for token in lexer.get_tokens(code.strip()):
    print(token)

# (Token.Operator, '%')
# (Token.Keyword, 'matplotlib')
# (Token.Text, ' inline\n')
```
