# fixme
export AWS_PROFILE := lambda
export AWS_REGION := ap-northeast-1

include build.mk

NAME ?= hello
FILE ?= $(NAME).json


# init
init:
	lambroll init --function-name $(NAME) --function=$(FILE) --region $(AWS_REGION)

# list
list:
	lambroll list

# deploy
deploy:
	lambroll deploy --function=$(FILE)

# logs
03:
	lambroll logs

# invoke
04:
	lambroll invoke --function=$(FILE)

setup:
	go get -v github.com/fujiwara/lambroll/cmd/lambroll

ROLE_NAME := lambda-ex

# get-role
get-role-arn: TARGET ?= lambda-ex
get-role-arn:
# これではattach roleの結果を見れない？
	aws --output json iam get-role --role-name $(TARGET) | jq .Role.Arn -r

# create role
x00: TARGET ?= file://trust-policy.json
x00:
	aws iam create-role --role-name $(ROLE_NAME) --assume-role-policy-document $(TARGET)

# attach role
x01:
	aws iam attach-role-policy --role-name $(ROLE_NAME) --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole

clean:
	lambroll delete --function $(FILE)
