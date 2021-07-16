## python pandas

難しい

- stack, unstack
- pivot
- pivot_table
- melt, unmelt

## dockerでproxyを使う方法

- https://docs.docker.com/network/proxy/

configを設定してあげれば良い見たい。

### pip

mitmした時のこのオプションをどうやって渡したら良いんだろう？

```console
$ pip install -U pip --trusted-host pypi.org --trusted-host files.pythonhosted.org
```

https://pip.pypa.io/en/stable/user_guide/#environment-variables

> pip’s command line options can be set with environment variables using the format PIP_<UPPER_LONG_NAME> . Dashes (-) have to be replaced with underscores (_).

### build-arg

https://blog.bitsrc.io/how-to-pass-environment-info-during-docker-builds-1f7c5566dd0e

```docker
ARG PIP_OPTION=
RUN pip3 install -U pip $PIP_OPTION && pip3 install $PIP_OPTION uvicorn
```

```console
$ docker build bar:0.0.0 . --build-arg PIP_OPTION='--trusted-host pypi.org --trusted-host files.pythonhosted.org'
```

## go forward proxy

- https://github.com/davidfstr/nanoproxy
- https://github.com/smartystreets/cproxy
- https://github.com/yuroyoro/mitm_proxy_sample
- https://github.com/elazarl/goproxy
- https://github.com/kitabisa/mubeng

nanoproxyは7年前。

### 普通にreverseproxyではダメなんだろうか？

CONNECT methodとか対応していないじゃん

### mitm

- https://github.com/elazarl/goproxy/blob/master/examples/goproxy-customca/main.go
- https://github.com/moriyoshi/devproxy
- https://gist.github.com/yuroyoro/c26ba2f34fee3b8398a987b2e39e9368

### pip でmitm

素直にmitmすると、verificationで引っかかる。

```
  WARNING: Retrying (Retry(total=1, connect=None, read=None, redirect=None, status=None)) after connection broken by 'SSLError
(SSLCertVerificationError(1, '[SSL: CERTIFICATE_VERIFY_FAILED] certificate verify failed: self signed certificate in certifica
te chain (_ssl.c:1129)'))': /packages/bf/fe/a41994c92897b162c0c83e8ef10bec54ebdefbce3f3725b530d2091492ac/uvicorn-0.14.0-py3-no
ne-any.whl
```

`--trust-url` など指定してあげるとスキップできる。強引っぽいけど。

## python pip proxy

- arm64用のwheelの名称を把握したい
- 存在しなかったらdocker buildする感じのものが欲しい
- (arm64用のbuildをやってくれるツール群／imageがありそう）
