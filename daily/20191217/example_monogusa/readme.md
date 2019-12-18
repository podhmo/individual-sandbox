# monogusa

https://github.com/podhmo/monogusa

## 引数の調査

とりあえず細かな調整をしたい。

- 環境変数以外の部分でdebugなどを調整できるようにする
- `--debug` をサブコマンド側でも使えるようにしつつ `--debug` でdebugの調整ができるように
- `-` を利用することにした

## CLIでも直接実行可能なようにする

webだけだとかなしいので。

## lifecycle event的なものを用意したい

starletteとかfastapiだとどうなってたっけ？

- https://www.starlette.io/events/
- https://fastapi.tiangolo.com/tutorial/events/

starletteは

```python
Starlette(on_startup=[fn], on_shutdown=[fn])
```

fastapiは

```python
@app.on_event("startup")
async def fn():
    pass
```

### 追記

自分の方ではどうしようかな。

```
@lifecycle.on_startup
async def fn():
    pass
```
