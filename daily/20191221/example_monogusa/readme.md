# monogusa

- https://github.com/podhmo/monogusa

## また少しずつ調整をしていく

たしか前回でかんたんなweb用のコードは出力できるようになった。
ただ出力関係がまだ標準出力ベースでこれは後々きつくなっていくことが予想される感じだった。

とりあえずprestring.output経由で出力して複数ファイルの出力ができるようにしておきたい。

## 未来の話

ちょっとだけ未来の話。

### on_startup, on_shutdown event

あと不足している部分はon_startup, on_shutdown的なevent (setup, teardownと見做しても良い)。なんで欲しくなるかというと例えばcomponentのconnect,disconnectのような処理を間にはさみたいから。コレがあるとCRUD的なインターフェイスのモジュールをそのままコマンドとして提供できるようになる。

crud.py

```python
async def read_notes(db: Database, *, completed_only:bool=True) -> List[Note]
    query = notes.select()
    return await db.fetch_all(query)
```

現状はそれがないのでCLI用のコマンドを作るために以下の様なコードが必要になる。

```python
async def list(db: Database) -> None:
    await db.connect()  # TODO: startup (lifecycle)
    print(await crud.read_notes(db))
```

これは、web上のAPIとしての公開を考えた時に嬉しくない。

```python
@app.get("/notes/", response_model=t.List[Note])
async def read_notes():
    return await crud.read_notes(db)

# 現状ではすべてcommand likeな処理のシェル実行をシミュレートしているので以下
from fastapi import Depends
from monogusa.web import runtime

@app.get("/notes/", response_model=runtime.CommandOutput)
async def read_notes(db: Database = Depends()):
    with runtime.handle() as s:
        await crud.read_notes(db)
        return s.dict()
```

### callback action

加えてcallback action (presenter) 的なものも本当は用意できておくと良い。コレがあると今までのようなcommand likeな関数 (write系) だけでなく、query likeな関数 (read系) も雑に公開できる余地が生まれる。とくにCLIとwebの相性的な話で。

雑に考えるならdefaultはprintで良いかもしれない。コレがあると完全にcrud.py的なものを作った時点で公開できるようになる。

これは実質crud.read_notesで良い :tada:

```python
async def list(db: Database, *, completed_only:bool=True) -> List[Note]
    return await crud.read_notes(db, completed_only=completed_only)
```

あるいは `--format=json` みたいなオプションを勝手にはやしても良いかもしれない。

### bulk action

あと地味に嫌なのがimportが重い系の処理を複数の対象に対して実行する必要が出てきた時。処理時間のほとんどはimportなどのload time。かなしい。

これを良い感じに実行する機能があると嬉しい。どういうインターフェイスにするかは決めあぐねていて。現状の思いつきのアイデアとしては標準入力を使うというもの (just my x cent 的なやつ)。

commands.txt

```
add "brush my teeth" --completed
add "read book"
add "goto bed"
```

こんな感じで実行する。dbへの接続/切断とかなんども繰り返したくないよね。。
(あとpandasのimportとか。。)

```console
$ cat commands.txt | python -m monogusa.cli crud.py
```

parseだけするようなdry-runオプションもあっても良いかもしれない。
