00:
	python $(shell echo $@*/)main.py token.json
01:
	python $(shell echo $@*/)main.py token.json

# need:
#
# create service account
# enable gdrive api
#
# gcloud auth login
# gcloud config set projec <project id>
02:
	gcloud functions deploy hello_http --source $(shell echo $@*/) --runtime python38 --trigger-http --allow-unauthenticated

# test
# gcloud functions describe hello_http
# open https://GCP_REGION-PROJECT_ID.cloudfunctions.net/hello_http?name=NAME

03:
	gcloud functions deploy hello_http --source $(shell echo $@*/) --runtime python38 --trigger-http --allow-unauthenticated
	# gcloud functions deploy hello_http --source $(shell echo $@*/) --runtime python38 --trigger-http --allow-unauthenticated --service-account=$(shell cat service_account)

req:
	@echo http https://us-central1-$(shell gcloud config get-value project 2>/dev/null).cloudfunctions.net/hello_http?name=NAME

clean:
	gcloud functions list
	gcloud functions delete hello_http
