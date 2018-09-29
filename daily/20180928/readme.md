## python js

js to pythonのtranpiler。

- https://github.com/PiotrDabkowski/Js2Py

ここで試せる

- http://piter.io/projects/js2py

### 追記

runtimeが微妙かも。runtimeのbootstrapに時間かかりすぎ。

### 追記

pyjsparserだけ使わせてもらう？

依存

```
{
  "packages": [
    {
      "name": "Js2Py",
      "_previous_version": "0.59",
      "version": "0.59",
      "last_modified": "2018-02-05T00:49:22"
    },
    {
      "name": "pyjsparser",
      "_previous_version": "2.5.2",
      "version": "2.5.2",
      "last_modified": "2017-07-09T11:31:50"
    },
    {
      "name": "pytz",
      "_previous_version": "2018.5",
      "version": "2018.5",
      "last_modified": "2018-06-29T06:53:04"
    },
    {
      "name": "six",
      "_previous_version": "1.11.0",
      "version": "1.11.0",
      "last_modified": "2017-09-17T18:46:53"
    },
    {
      "name": "tzlocal",
      "_previous_version": "1.5.1",
      "version": "1.5.1",
      "last_modified": "2017-12-01T09:37:55"
    }
  ],
  "update_candidates": [],
  "new_install_candidates": [],
  "dependencies": [
    {
      "Js2Py": [
        "six",
        "pyjsparser",
        {
          "tzlocal": [
            "pytz"
          ]
        }
      ]
    },
    {
      "tzlocal": [
        "pytz"
      ]
    }
  ]
}

```

## python python-semver

メンテがめんどくさくなってきた

https://github.com/podhmo/python-semver
