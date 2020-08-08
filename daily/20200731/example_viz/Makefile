GEN_DIAGRAM := pyreverse -o svg --all-ancestors --all-associated -f ALL

# layout
# layout 	description
# circo 	円形のグラフ.
# dot 	階層型のグラフ. 有向グラフ向き. デフォルトのレイアウトエンジン
# fdp 	スプリング(ばね)モデルのグラフ. 無向グラフ向き.
# neato 	スプリング(ばね)モデルのグラフ. 無向グラフ向き.
# osage 	配列型のグラフ.
# sfdp 	fdpのマルチスケール版. 大きな無向グラフ向き.
# twopi 放射型のグラフ. ノードは同心円状に配置される.	

00:
	$(GEN_DIAGRAM) -p simple $(shell echo $@*/)*.py
01:
	$(GEN_DIAGRAM) -p with-init $(shell echo $@*/)*.py
02:
	dot -Tsvg $(shell echo $@*.dot) -O 
03:
	dot -Tsvg $(shell echo $@*.dot) -O 
04:
	dot -Tsvg $(shell echo $@*.dot) -O 
05:
	dot -Kdot -Tsvg $(shell echo $@*.dot) -O 
06:
	dot -Kdot -Tsvg $(shell echo $@*.dot) -O 

setup:
	pip install -U pylint
