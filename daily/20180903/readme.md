## python gloudの情報を取り出す

```
$ ls ~/.config/gcloud/credentials.db
$ sqlite3 ~/.config/gcloud/credentials.db 'select * from credentials;'
```

## python gcloud sdkをpython3.xの環境にインストールする

```
# CLOUDSDK_PYTHON=`which python` --disable-prompts --install-dir=${VIRTUAL_ENV}/lib/python`python -c 'import sys; print(".".join(map(str,sys.version_info[:2])))'`
# CLOUDSDK_PYTHON=`which python` --disable-prompts --install-dir=${VIRTUAL_ENV}

$ CLOUDSDK_PYTHON=`which python` bash -x ./install.sh
$ gcloud init
$ gcloud config list
```

- https://cloud.google.com/sdk/docs/downloads-interactive

memo

```
replace '\.async' '.aasync'
replace 'aasyncio'  'asyncio'
```

## js prettier

install

```console
$ npm install -g prettier
# or yarn
```

write

```console
$ prettier --single-quote --trailing-comma es5 --write "{app,__{tests,mocks}__}/**/*.js"
```

### see also

https://prettier.io/docs/en/install.html

