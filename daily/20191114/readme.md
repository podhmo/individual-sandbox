## metashape python

とりあえず以下のことができるようになって欲しい

- strict=TrueでadditionalProperties:falseが付く
- オプションを自由に渡せる
- emitの手前の状態がscanで手にはいる

### 追記

そもそもinternalなcontextの生成がwalkerからやってくるというのが変なのかもしれない。
現状のinternalなcontextは以下のようなオプションを持っているので。

```python
    class Option:
        strict: bool = True
        verbose: bool = False
        ignore_private: bool = True
        output_format: str = "json"
```

そういえばstrictの意味が微妙に違うのか。strict_outputとかつくる？
他にstrictを使っている箇所がなかったのでそのまま使うことにした。

additionalProperties=Falseをdefaultにした。このへんの対応手軽にできるといいつつ生成担っているのがだるい。

## 追記

次はどの辺りをしようかな。
