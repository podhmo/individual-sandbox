01:
	$(MAKE) -s ranking | tee ranking.md
02:
	$(MAKE) -s ranking TARGET=_ranking2 | tee ranking2.md
03:
	$(MAKE) -s ranking TARGET=_ranking3 | tee ranking3.md


ranking: TARGET ?= _ranking
ranking: OPT ?= -f json
ranking:
	$(MAKE) ${TARGET} CMD=recent OPT="${OPT}" | jqfpy -c 'd = get(); {"package": d["package"], **d["data"]}' | jqfpy --slurp 'sorted(get(), key=lambda x: x["last_month"] + (4 * x["last_week"]), reverse=True)' | dictknife cat -i json -o md

_ranking: OPT ?= -f json
_ranking: CMD ?= recent
_ranking:
	@pypistats ${CMD} ${OPT} matplotlib
	@pypistats ${CMD} ${OPT} altair
	@pypistats ${CMD} ${OPT} seaborn
	@pypistats ${CMD} ${OPT} plotnine
	@pypistats ${CMD} ${OPT} plotly
	@pypistats ${CMD} ${OPT} yellowbrick
	@pypistats ${CMD} ${OPT} ggplot
	@pypistats ${CMD} ${OPT} pygal
	@pypistats ${CMD} ${OPT} pygraphviz
	@pypistats ${CMD} ${OPT} pydot
# interactive?
	@pypistats ${CMD} ${OPT} vega
	@pypistats ${CMD} ${OPT} vincent
	@pypistats ${CMD} ${OPT} bokeh
	@pypistats ${CMD} ${OPT} mpld3
	@pypistats ${CMD} ${OPT} mplcursors

_ranking2: OPT ?= -f json
_ranking2: CMD ?= recent
_ranking2:
	@pypistats ${CMD} ${OPT} tensorflow
	@pypistats ${CMD} ${OPT} keras
	@pypistats ${CMD} ${OPT} pytorch
	@pypistats ${CMD} ${OPT} chainer

_ranking3: OPT ?= -f json
_ranking3: CMD ?= recent
_ranking3:
	@pypistats ${CMD} ${OPT} statsmodels
	@pypistats ${CMD} ${OPT} gensim
	@pypistats ${CMD} ${OPT} spacy
	@pypistats ${CMD} ${OPT} nltk
	@pypistats ${CMD} ${OPT} numpy
	@pypistats ${CMD} ${OPT} scipy
	@pypistats ${CMD} ${OPT} sympy
	@pypistats ${CMD} ${OPT} simpy
	@pypistats ${CMD} ${OPT} zipline
	@pypistats ${CMD} ${OPT} networkx
	@pypistats ${CMD} ${OPT} pymc
	@pypistats ${CMD} ${OPT} pyro
	@pypistats ${CMD} ${OPT} Edward
	@pypistats ${CMD} ${OPT} scikit-learn
	@pypistats ${CMD} ${OPT} xgboost
	@pypistats ${CMD} ${OPT} lightgbm
	@pypistats ${CMD} ${OPT} cvxopt
	@pypistats ${CMD} ${OPT} pulp
