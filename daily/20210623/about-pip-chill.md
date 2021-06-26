#[python][pip]pip-chillのススメ

[https://pypi.org/project/pip-chill:cite:embed]

`pip freeze` で出力されるすべてをrequirements.txtに書くのではなく、依存の一番外側のパッケージだけを出力したい事がある。そういうときにはpip-chillが便利。

## 実行例

例えば、fastapiを参考に以下の様なrequirements.txtを用意してみる。

requirements.txt

```
uvicorn
fastapi
```

空のvenvを作ってpip installしてみる

```console
$ python -m venv .venv
$ ./venv/bin/pip install -r requirements.txt
```console

### freeze

freezeの内容は以下

```console
$ ./venv/bin/pip freeze
asgiref==3.3.4
click==8.0.1
colorama==0.4.4
fastapi==0.65.2
h11==0.12.0
pip-chill==1.0.1
pydantic==1.8.2
starlette==0.14.2
typing-extensions==3.10.0.0
uvicorn==0.14.0
```

### pip-chill

pip-chillを使うと依存の一番外側だけを出力してくれる。

```console
$ ./venv/bin/pip install pip-chill
$ ./venv/bin/pip-chill --no-chill
fastapi==0.65.2
uvicorn==0.14.0
```

versionも削れる

```console
$ ./venv/bin/pip-chll --no-chll --no-version
fastapi
uvicorn
```


`pip install fastapi[dev]` した場合には以下の様に変わる

```console
$ ./venv/bin/pip install fastapi[dev]
$ ./venv/bin/pip-chill --no-chill
asgiref==3.3.4
autoflake==1.4
bcrypt==3.2.0
colorama==0.4.4
cryptography==3.4.7
fastapi==0.65.2
flake8==3.9.2
graphene==2.1.8
passlib==1.7.4
python-dotenv==0.18.0
python-jose==3.3.0
pyyaml==5.4.1
uvicorn==0.13.4
watchgod==0.7
websockets==8.1
```
