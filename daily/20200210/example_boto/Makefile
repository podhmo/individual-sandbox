00:
	python $(shell echo $@*.py)
01:
	python $(shell echo $@*.py)
02:
	dictknife shape $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') | tee sqs.shape
	dictknife shape $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/lambda/2015-03-31/service-2.json")') | tee lambda.shape

03:
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/operations/CreateQueue" -o json | tee sqs.CreateQueue.json
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/shapes/CreateQueueRequest" -o json | tee sqs.CreateQueueRequest.json
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/shapes/CreateQueueResult" -o json | tee sqs.CreateQueueResult.json
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/operations/SendMessage" -o json | tee sqs.SendMessage.json
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/shapes/SendMessageRequest" -o json | tee sqs.SendMessageRequest.json
	jsonknife select --src $(shell python -c 'import site; print(site.getsitepackages()[0] + "/botocore/data/sqs/2012-11-05/service-2.json")') --ref "#/shapes/SendMessageResult" -o json | tee sqs.SendMessageResult.json

04:
	$(MAKE) 01 -s | xargs grep '"protocol"' -h | sed 's/^ *//g; s/ *//g; s/,$$//g' | sort | uniq -c | sort -n

# build service
05:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
# subset
06:
	python $(shell echo $@*.py)| tee $(patsubst %.py,%.output,$(shell echo $@*.py))
# subset
07:
	python $(shell echo $@*.py)| tee $(patsubst %.py,%.output,$(shell echo $@*.py))
