SERVER_PORT ?= 8888


# datasetの一覧を返す
00:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
# describeしてみる
01:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
# テキトーにgroupbyしてみる
02:
	$(foreach x,$(shell cat 00*.output),mkdir -p $(shell echo $@*/)$(patsubst %.json,%,$(x));)
	python $(shell echo $@*/*.py) \
 --dataset iris \
 --outputfile $(shell echo $@*/)cars/species-sepalLength.csv \
 --groupby species \
 --field sepalLength \
 --fns min \
 --fns max \
 --fns mean \
 --fns std \
 --fns count \

	python $(shell echo $@*/*.py) \
 --dataset iris \
 --outputfile $(shell echo $@*/)iris/species-sepalWidth.csv \
 --groupby species \
 --field sepalWidth \
 --fns min \
 --fns max \
 --fns mean \
 --fns std \
 --fns count \

	python $(shell echo $@*/*.py) \
 --dataset cars \
 --outputfile $(shell echo $@*/)cars/Year-Horsepower.csv \
 --groupby Year \
 --field Horsepower \
 --fns min \
 --fns max \
 --fns mean \
 --fns std \
 --fns count \


setup:
	pip install -r requirements.txt

# http :8888/dataset
# http :8888/dataset/iris
# http :8888/dataset/iris/describe
# http :8888/dataset/iris/columns
# http :8888/dataset/iris/groupby/species/aggs/sepalLength
# http :8888/dataset/cars/groupby/Year/aggs/Horsepower fn==max fn==min fn==count
server:
	uvicorn api:app --port $(SERVER_PORT)
