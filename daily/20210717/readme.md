## python dis

- dis.BytecodeでInstruction objectが取れるのでこれで管理できそう
- opnameでどんなものが呼び出せるかはわかる
- どのようなクラスが使われているかなどは、argvalあたりを使えば良さそう

  - 文字列で返ってくる

- どの行でエラーになったかの情報も存在していそう。

### 参考情報

- [Understanding Python Bytecode. Learn about disassembling Python… | by Reza Bagheri | Towards Data Science](https://towardsdatascience.com/understanding-python-bytecode-e7edaae8734d)
- [Bytecode Hacking for Great Justice](https://blog.fastforwardlabs.com/2015/04/23/bytecode-hacking-for-great-justice.html)
- https://github.com/bravegnu/python-byte-code/blob/master/Python%20Byte%20Code%20Hacking.ipynb

ここでopcodeの一覧が見れるのは便利かも

- http://unpyc.sourceforge.net/Opcodes.html

### 簡単に扱えそうなコードを見てみる

```
python 00use-object.py | grep "^@" | cut -d " " -f 4 | sort | uniq -c | sort
      1 BINARY_ADD
      1 BUILD_LIST
      1 CALL_METHOD
      1 LOAD_METHOD
      1 POP_TOP
      1 RETURN_VALUE
      2 LOAD_CONST
      3 STORE_FAST
      4 CALL_FUNCTION
      5 LOAD_GLOBAL
      8 LOAD_ATTR
     10 LOAD_FAST	
```

### 引数と変数の違いを調べる

```py
def inc(v:int) -> int:
    d = 1
    r = v + d
    return r
```

```
  4           0 LOAD_CONST               1 (1)
              2 STORE_FAST               1 (d)

  5           4 LOAD_FAST                0 (v)
              6 LOAD_FAST                1 (d)
              8 BINARY_ADD
             10 STORE_FAST               2 (r)

  6          12 LOAD_FAST                2 (r)
             14 RETURN_VALUE
```

- GLOBALはそのまま、globalsから取り出すものだろう
- FASTはどうやら引数などのもののようだ。
- 定数を読み込むときはCONST
- 変数と引数はどうやって区別すれば良いんだろう？
- そういえば、たぶんスタックマシンだよね。
- STORE_FASTのないものは引数なようだ。

  - 呼び出し元でコールフレームを作って実行するのだからそれはそうかも。