# fixme
export AWS_PROFILE := lambda
export AWS_REGION := ap-northeast-1

# init
00:
	lambroll init --function-name hello --download
# list
01:
	lambroll list

# deploy
02:
	lambroll deploy

# logs
03:
	lambroll logs

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
	lambroll delete
