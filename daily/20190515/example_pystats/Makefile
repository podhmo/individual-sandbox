default: 01 02 03

01:
	$(MAKE) -s ranking | tee ranking.md
11:
	$(MAKE) -s prev  | tee prev-ranking.md
02:
	$(MAKE) -s ranking TARGET=_ranking2 | tee ranking2.md
03:
	$(MAKE) -s ranking TARGET=_ranking3 | tee ranking3.md
04:
	$(MAKE) -s ranking TARGET=_ranking4 | tee ranking4.md

ranking: TARGET ?= _ranking
ranking: OPT ?= -f json
ranking:
	$(MAKE) ${TARGET} CMD=recent OPT="${OPT}" | jqfpy -c 'd = get(); {"package": d["package"], **d["data"]}' | jqfpy --slurp 'sorted(get(), key=lambda x: x["last_month"] + (4 * x["last_week"]), reverse=True)' | dictknife cat -i json -o md

prev: TARGET ?= _ranking
prev: OPT ?= -f json -m 2018-12
prev:
	$(MAKE) ${TARGET} CMD=overall OPT="${OPT}" | jqfpy -c 'd = get(); {"package": d["package"], "prev": sum([x["downloads"] for x in d["data"]])}' | jqfpy --slurp 'sorted(get(), key=lambda x: x["prev"], reverse=True)' | dictknife cat -i json -o md

_ranking: OPT ?= -f json
_ranking: CMD ?= recent
_ranking:
	@pypistats ${CMD} ${OPT} flask
	@pypistats ${CMD} ${OPT} django
	@pypistats ${CMD} ${OPT} tornado
	@pypistats ${CMD} ${OPT} pyramid
	@pypistats ${CMD} ${OPT} bottle
	@pypistats ${CMD} ${OPT} responder
	@pypistats ${CMD} ${OPT} fastapi
	@pypistats ${CMD} ${OPT} starlette
	@pypistats ${CMD} ${OPT} sanic
	@pypistats ${CMD} ${OPT} falcon
	@pypistats ${CMD} ${OPT} masonite
	@pypistats ${CMD} ${OPT} vibora
	@pypistats ${CMD} ${OPT} molten
	@pypistats ${CMD} ${OPT} cherrypy
	@pypistats ${CMD} ${OPT} web2py
	@pypistats ${CMD} ${OPT} aiohttp
	@pypistats ${CMD} ${OPT} muffin
	@pypistats ${CMD} ${OPT} weppy
	@pypistats ${CMD} ${OPT} quart
	@pypistats ${CMD} ${OPT} morepath
	@pypistats ${CMD} ${OPT} hug
	@pypistats ${CMD} ${OPT} growler
	@pypistats ${CMD} ${OPT} circuits
	@pypistats ${CMD} ${OPT} TurboGears2
	@pypistats ${CMD} ${OPT} bocadillo
# serverless
	@pypistats ${CMD} ${OPT} chalice
	@pypistats ${CMD} ${OPT} zappa
# wsgi / asgi (infra)
	@pypistats ${CMD} ${OPT} werkzeug
	@pypistats ${CMD} ${OPT} webob
	@pypistats ${CMD} ${OPT} waitress
	@pypistats ${CMD} ${OPT} gunicorn
	@pypistats ${CMD} ${OPT} uvicorn
# doping
	@pypistats ${CMD} ${OPT} uwsgi
	@pypistats ${CMD} ${OPT} uvloop
	@pypistats ${CMD} ${OPT} meinheld
	@pypistats ${CMD} ${OPT} ujson

_ranking2: OPT ?= -f json
_ranking2:
	@pypistats recent ${OPT} sqlalchemy
	@pypistats recent ${OPT} orator
	@pypistats recent ${OPT} peewee
	@pypistats recent ${OPT} pony
	@pypistats recent ${OPT} dataset
	@pypistats recent ${OPT} records
	@pypistats recent ${OPT} storm
	@pypistats recent ${OPT} SQLObject
# specific
	@pypistats recent ${OPT} gino
	@pypistats recent ${OPT} pydal
	@pypistats recent ${OPT} databases
# driver
	@pypistats recent ${OPT} asyncpg
	@pypistats recent ${OPT} psycopg2
	@pypistats recent ${OPT} pymysql
	@pypistats recent ${OPT} mysql-connector-python
	@pypistats recent ${OPT} MySQL-python
	@pypistats recent ${OPT} asyncorm
	@pypistats recent ${OPT} aiomysql
	@pypistats recent ${OPT} aiosqlite
# mongo
	@pypistats recent ${OPT} pymongo
	@pypistats recent ${OPT} mongoengine
	@pypistats recent ${OPT} umongo
	@pypistats recent ${OPT} motor
	@pypistats recent ${OPT} mongoframes
# comparsion
	@pypistats recent ${OPT} pandas
	@pypistats recent ${OPT} pyspark

_:
	@pypistats recent ${OPT} numpy
	@pypistats recent ${OPT} scipy
	@pypistats recent ${OPT} matplotlib
	@pypistats recent ${OPT} sympy

_ranking3: OPT ?= -f json
_ranking3:
	@pypistats recent ${OPT} marshmallow
	@pypistats recent ${OPT} pydantic
	@pypistats recent ${OPT} trafaret
	@pypistats recent ${OPT} voluptuous
	@pypistats recent ${OPT} cerberus
	@pypistats recent ${OPT} schema
	@pypistats recent ${OPT} schematics
	@pypistats recent ${OPT} middle
# forms
	@pypistats recent ${OPT} wtforms
	@pypistats recent ${OPT} colander
	@pypistats recent ${OPT} formencode
# hmm
	@pypistats recent ${OPT} dataclasses
	@pypistats recent ${OPT} djangorestframework
	@pypistats recent ${OPT} jsonschema

_ranking4: OPT ?= -f json
_ranking4:
	@pypistats recent ${OPT} flake8
	@pypistats recent ${OPT} pycodestyle
	@pypistats recent ${OPT} pyflakes
	@pypistats recent ${OPT} pylint
	@pypistats recent ${OPT} hacking
# test
	@pypistats recent ${OPT} nose
	@pypistats recent ${OPT} nose2
	@pypistats recent ${OPT} pytest
	@pypistats recent ${OPT} green
	@pypistats recent ${OPT} coverage
	@pypistats recent ${OPT} tox
	@pypistats recent ${OPT} fixtures
	@pypistats recent ${OPT} factory_boy
	@pypistats recent ${OPT} faker
	@pypistats recent ${OPT} mimesis
	@pypistats recent ${OPT} hypothesis
# mock
	@pypistats recent ${OPT} mock
	@pypistats recent ${OPT} requests-mock
	@pypistats recent ${OPT} vcrpy
# fmt
	@pypistats recent ${OPT} autoflake
	@pypistats recent ${OPT} isort
	@pypistats recent ${OPT} autopep8
	@pypistats recent ${OPT} yapf
	@pypistats recent ${OPT} black
# ci
	@pypistats recent ${OPT} mccabe
	@pypistats recent ${OPT} radon
	@pypistats recent ${OPT} xenon
# typing
	@pypistats recent ${OPT} mypy
	@pypistats recent ${OPT} typing_extensions
	@pypistats recent ${OPT} mypy_extensions
	@pypistats recent ${OPT} pyannotate
	@pypistats recent ${OPT} monkeytype
# flake8 plugin
	@pypistats recent ${OPT} flake8-alfred
	@pypistats recent ${OPT} flake8-annotations-complexity
	@pypistats recent ${OPT} flake8-annotations-coverage
	@pypistats recent ${OPT} flake8-assertive
	@pypistats recent ${OPT} flake8-author
	@pypistats recent ${OPT} flake8-awesome
	@pypistats recent ${OPT} flake8-bandit
	@pypistats recent ${OPT} flake8-black
	@pypistats recent ${OPT} flake8-blind-except
	@pypistats recent ${OPT} flake8-breakpoint
	@pypistats recent ${OPT} flake8-broken-line
	@pypistats recent ${OPT} flake8-bugbear
	@pypistats recent ${OPT} flake8-builtins-unleashed
	@pypistats recent ${OPT} flake8-chart
	@pypistats recent ${OPT} flake8-checkstyle
	@pypistats recent ${OPT} flake8-class-newline
	@pypistats recent ${OPT} flake8-codeclimate
	@pypistats recent ${OPT} flake8-coding
	@pypistats recent ${OPT} flake8-colors
	@pypistats recent ${OPT} flake8-commas
	@pypistats recent ${OPT} flake8-comprehensions
	@pypistats recent ${OPT} flake8-confusables
	@pypistats recent ${OPT} flake8-copyright
	@pypistats recent ${OPT} flake8-csv
	@pypistats recent ${OPT} flake8-datetimez
	@pypistats recent ${OPT} flake8-debugger
	@pypistats recent ${OPT} flake8-diff
	@pypistats recent ${OPT} flake8-disrespectful-code
	@pypistats recent ${OPT} flake8-docstrings
	@pypistats recent ${OPT} flake8-docstrings-catnado
	@pypistats recent ${OPT} flake8-double-quotes
	@pypistats recent ${OPT} flake8-eradicate
	@pypistats recent ${OPT} flake8-expandtab
	@pypistats recent ${OPT} flake8-filename
	@pypistats recent ${OPT} flake8-fixme
	@pypistats recent ${OPT} flake8-format-ansi
	@pypistats recent ${OPT} flake8-formatter-abspath
	@pypistats recent ${OPT} flake8-formatter-junit-xml
	@pypistats recent ${OPT} flake8-future
	@pypistats recent ${OPT} flake8-future-import
	@pypistats recent ${OPT} flake8-graphql
	@pypistats recent ${OPT} flake8-html
	@pypistats recent ${OPT} flake8-illegal-import
	@pypistats recent ${OPT} flake8-immediate
	@pypistats recent ${OPT} flake8-import-order
	@pypistats recent ${OPT} flake8-imports
	@pypistats recent ${OPT} flake8-import-single
	@pypistats recent ${OPT} flake8-import-style
	@pypistats recent ${OPT} flake8-isort
	@pypistats recent ${OPT} flake8-json
	@pypistats recent ${OPT} flake8-junit-report
	@pypistats recent ${OPT} flake8-libfaketime
	@pypistats recent ${OPT} flake8-license
	@pypistats recent ${OPT} flake8-logging-format
	@pypistats recent ${OPT} flake8-module-imports
	@pypistats recent ${OPT} flake8-mutable
	@pypistats recent ${OPT} flake8-mypy
	@pypistats recent ${OPT} flake8-network-timeout
	@pypistats recent ${OPT} flake8-nitpick
	@pypistats recent ${OPT} flake8-ownership
	@pypistats recent ${OPT} flake8-patch-decorator-unleashed
	@pypistats recent ${OPT} flake8-pep257
	@pypistats recent ${OPT} flake8-phabricator-formatter
	@pypistats recent ${OPT} flake8-pie
	@pypistats recent ${OPT} flake8-plugin-utils
	@pypistats recent ${OPT} flake8-polyfill
	@pypistats recent ${OPT} flake8-print
	@pypistats recent ${OPT} flake8-putty
	@pypistats recent ${OPT} flake8-pyi
	@pypistats recent ${OPT} flake8-pytest
	@pypistats recent ${OPT} flake8-pytest-mark
	@pypistats recent ${OPT} flake8-quotes
	@pypistats recent ${OPT} flake8-quotes2
	@pypistats recent ${OPT} flake8-regex
	@pypistats recent ${OPT} flake8-requirements
	@pypistats recent ${OPT} flake8-respect-noqa
	@pypistats recent ${OPT} flake8-return
	@pypistats recent ${OPT} flake8-rewriter
	@pypistats recent ${OPT} flake8-rst
	@pypistats recent ${OPT} flake8-self
#	@pypistats recent ${OPT} flake8-setuptools
	@pypistats recent ${OPT} flake8-single-quotes
	@pypistats recent ${OPT} flake8-snippets
	@pypistats recent ${OPT} flake8-SQL
	@pypistats recent ${OPT} flake8-string-format
	@pypistats recent ${OPT} flake8-super-call
	@pypistats recent ${OPT} flake8-tabs
	@pypistats recent ${OPT} flake8-tidy-imports
	@pypistats recent ${OPT} flake8-todo
	@pypistats recent ${OPT} flake8-trailing-commas
	@pypistats recent ${OPT} flake8-translation-activate
	@pypistats recent ${OPT} flake8-type-annotations
	@pypistats recent ${OPT} flake8-ugettext-alias
	@pypistats recent ${OPT} flake8-user-model
	@pypistats recent ${OPT} flake8-variables-names
	@pypistats recent ${OPT} flake8-vyper
	@pypistats recent ${OPT} flake8-ycm
	@pypistats recent ${OPT} sentry-flake8
# pytest plugin
	@pypistats recent ${OPT} pytest-aiohttp
	@pypistats recent ${OPT} pytest-airflow
	@pypistats recent ${OPT} pytest-annotate
	@pypistats recent ${OPT} pytest-apistellar
	@pypistats recent ${OPT} pytest-assertutil
	@pypistats recent ${OPT} pytest-asyncio
	@pypistats recent ${OPT} pytest-bandit
	@pypistats recent ${OPT} pytest-bdd
	@pypistats recent ${OPT} pytest-bench
# 	@pypistats recent ${OPT} pytest-bigchaindb
	@pypistats recent ${OPT} pytest-codestyle
	@pypistats recent ${OPT} pytest-cookies
	@pypistats recent ${OPT} pytest-cov
	@pypistats recent ${OPT} pytest-cover
	@pypistats recent ${OPT} pytest-cram
	@pypistats recent ${OPT} pytest-cricri
	@pypistats recent ${OPT} pytest-csv
	@pypistats recent ${OPT} pytest-curio
	@pypistats recent ${OPT} pytest-datatest
	@pypistats recent ${OPT} pytest-describe
	@pypistats recent ${OPT} pytest-diamond
	@pypistats recent ${OPT} pytest-diff
	@pypistats recent ${OPT} pytest-django
	@pypistats recent ${OPT} pytest-docs
	@pypistats recent ${OPT} pytest-docstyle
	@pypistats recent ${OPT} pytest-dynamodb
	@pypistats recent ${OPT} pytest-ethereum
	@pypistats recent ${OPT} pytest-excel
	@pypistats recent ${OPT} pytest-expecter
	@pypistats recent ${OPT} pytest-factoryboy
	@pypistats recent ${OPT} pytest-faker
	@pypistats recent ${OPT} pytest-falcon
	@pypistats recent ${OPT} pytest-fauxfactory
	@pypistats recent ${OPT} pytest-firefox
	@pypistats recent ${OPT} pytest-fixtures
	@pypistats recent ${OPT} pytest-flake8dir
	@pypistats recent ${OPT} pytest-fxa
	@pypistats recent ${OPT} pytest-growl
	@pypistats recent ${OPT} pytest-grpc
	@pypistats recent ${OPT} pytest-html
	@pypistats recent ${OPT} pytest-httpretty
	@pypistats recent ${OPT} pytest-httpserver
	@pypistats recent ${OPT} pytest-invenio
	@pypistats recent ${OPT} pytest-jest
	@pypistats recent ${OPT} pytest-libfaketime
	@pypistats recent ${OPT} pytest-localstack
	@pypistats recent ${OPT} pytest-mongodb
	@pypistats recent ${OPT} pytest-monkeyplus
	@pypistats recent ${OPT} pytest-monkeytype
	@pypistats recent ${OPT} pytest-neo
	@pypistats recent ${OPT} pytest-nginx
	@pypistats recent ${OPT} pytest-ngsfixtures
	@pypistats recent ${OPT} pytest-ok
	@pypistats recent ${OPT} pytest-optional
	@pypistats recent ${OPT} pytest-orchestration
	@pypistats recent ${OPT} pytest-pact
	@pypistats recent ${OPT} pytest-pdb
	@pypistats recent ${OPT} pytest-pep8
	@pypistats recent ${OPT} pytest-ponyorm
	@pypistats recent ${OPT} pytest-pudb
	@pypistats recent ${OPT} pytest-pycodestyle
	@pypistats recent ${OPT} pytest-pydocstyle
	@pypistats recent ${OPT} pytest-pyq
	@pypistats recent ${OPT} pytest-pytestrail
	@pypistats recent ${OPT} pytest-race
	@pypistats recent ${OPT} pytest-rage
	@pypistats recent ${OPT} pytest-reana
	@pypistats recent ${OPT} pytest-redmine
	@pypistats recent ${OPT} pytest-regtest
	@pypistats recent ${OPT} pytest-repeat
	@pypistats recent ${OPT} pytest-rethinkdb
	@pypistats recent ${OPT} pytest-rt
	@pypistats recent ${OPT} pytest-runfailed
	@pypistats recent ${OPT} pytest-salt
	@pypistats recent ${OPT} pytest-sanic
	@pypistats recent ${OPT} pytest-scenario
	@pypistats recent ${OPT} pytest-selenium
	@pypistats recent ${OPT} pytest-slack
	@pypistats recent ${OPT} pytest-smartcov
	@pypistats recent ${OPT} pytest-sourceorder
	@pypistats recent ${OPT} pytest-splinter
	@pypistats recent ${OPT} pytest-statsd
	@pypistats recent ${OPT} pytest-sugar
	@pypistats recent ${OPT} pytest-symbols
	@pypistats recent ${OPT} pytest-testconfig
	@pypistats recent ${OPT} pytest-testdox
	@pypistats recent ${OPT} pytest-tesults
	@pypistats recent ${OPT} pytest-toolbox
	@pypistats recent ${OPT} pytest-trepan
	@pypistats recent ${OPT} pytest-trio
	@pypistats recent ${OPT} pytest-warnings
	@pypistats recent ${OPT} pytest-xpara
	@pypistats recent ${OPT} pytest-zafira

