PROJECT_ID := speedy-equator-284105
IMAGE_NAME := helloworld
SERVICE_NAME := helloworld

list:
	gcloud projects list
build:
	gcloud builds submit --tag gcr.io/$(PROJECT_ID)/$(IMAGE_NAME)
deploy:
	gcloud run deploy $(SERVICE_NAME) --image gcr.io/$(PROJECT_ID)/$(IMAGE_NAME) --platform managed --region asia-northeast1 # \
 # --allow-unauthenticated

check-iam:
	gcloud run services get-iam-policy $(SERVICE_NAME) --region=asia-northeast1 --platform=managed

https://cloud.google.com/sdk/gcloud/reference/projects/add-iam-policy-binding
set-iam: USER ?= user:ababjam61@gmail.com
set-iam:
	gcloud run services add-iam-policy-binding $(SERVICE_NAME) --region=asia-northeast1 --member=$(USER) --role=roles/run.invoker

run-local: PORT ?= 8888
run-local:
	uvicorn app:app --reload --log-level=debug --port=$(PORT)

# setup
config:
	gcloud config list

config-setup: NAME ?= new
config-setup: EMAIL ?= ababjam61@gmail.com
config-setup:
	# gcloud config configurations list
	gcloud config configurations activate $(NAME)
	gcloud config set compute/region asia-northeast1
	gcloud config set compute/zone asia-northeast1-a
	gcloud config set core/account $(EMAIL)
	gcloud config set core/project $(PROJECT_ID)
	gcloud config set core/disable_usage_reporting False
