## pypistatsなどのデータの意味

- pypistats は https://pypistats.org/api/

resourceは

https://pypistats.org/faqs#what-is-the-source-of-the-download-data

> PyPI provides download records as a publicly available dataset on Google's BigQuery. You can access the data with a Google Cloud account here. 

### 個別に見れる

- https://pypistats.org/packages/responder
- https://pypistats.org/packages/fastapi

## pystatsが便利

- flask,django,bottle,pyramid,falcon,responder,fastapi,starlette,masonite,sanic,aiohttp
- yapf,autpep8,black
- mypy,flake8,pycodestyle,pep8,pyflakes

### あらく人気を集める

```console
PKG=flask
pypistats overall -m 2019-01 ${PKG}
pypistats overall -m 2019-02 ${PKG}
pypistats overall -m 2019-03 ${PKG}
pypistats overall -m 2019-04 ${PKG}
pypistats overall -m 2019-05 ${PKG}
```

## pythonのpackageのversionごとのdownload率をみたい

- https://github.com/hugovk/pypistats
- https://github.com/ofek/pypinfo

```
$ pypistats python_major dvc --last-month # pip install pypistats
$ pypinfo --start-date 2017-06-01 --end-date 2018-05-31 --percent --markdown pymagicc pyversion
```

## bigqueryで結果を見る

- https://cloud.google.com/bigquery/public-data/github
- https://bigquery.cloud.google.com/dataset/bigquery-public-data:github_repos

pypinfo --start-date 2017-06-01 --end-date 2018-05-31 --percent --markdown pymagicc pyversion

githubのことしか書かれていないか？

## pythonだけなら

## pypiinfo

```console
$ pip install pypinfo
$ pypinfo --auth <path/to/your_credentials.json> <>
# or GOOGLE_APPLICATION_CREDENTIALS=<path/to/your_credentials.json>
```

hmm

```
pypistats python_major dvc --last-month # pip install pypistats
```

[pypinfo](https://github.com/ofek/pypinfo)でいけそう

- https://github.com/hugovk/pypi-tools
- https://github.com/hugovk/top-pypi-packages
- https://github.com/ofek/pypinfo

