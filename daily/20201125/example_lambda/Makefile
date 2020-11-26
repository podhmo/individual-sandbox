# https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/gettingstarted-awscli.html

SHELL := bash
# FIXME
export AWS_PROFILE := lambda

# list lambda function
list-function:
	aws lambda list-functions --max-items 10

# list-role
list-role:
	aws iam list-roles
# get-role
get-role: TARGET ?= lambda-ex
get-role:
# これではattach roleの結果を見れない？
	aws --output json iam get-role --role-name $(TARGET)

identity:
	aws sts get-caller-identity

########################################
# try lambda
########################################

ROLE_NAME := lambda-ex
FUNCTION_NAME := my-function3

# create role
00: TARGET ?= file://trust-policy.json
00:
	aws iam create-role --role-name $(ROLE_NAME) --assume-role-policy-document $(TARGET)

# attach role
01:
	aws iam attach-role-policy --role-name $(ROLE_NAME) --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole


get-role-arn:
# これではattach roleの結果を見れない？
	aws --output json iam get-role --role-name $(ROLE_NAME) | jq .Role.Arn -r

# create function
02: function.zip
	aws lambda create-function --function-name $(FUNCTION_NAME) \
--zip-file fileb://function.zip --handler index.handler --runtime nodejs12.x \
--role $(shell make -s get-role-arn)

# invoke function
03:
	aws lambda invoke --function-name $(FUNCTION_NAME) out --log-type Tail \
--query 'LogResult' --output text |  base64 -D # mac is -D linux is -d

# # check logs
# 04: TARGET ?= /aws/lambda/$(FUNCTION_NAME)
# 04:
# 	aws logs describe-log-streams --log-group-name $(TARGET) 
# 	# aws logs get-log-events --log-group-name $(TARGET) --log-stream-name  --limit 5

# get function
05:
	$ aws lambda get-function --function-name $(FUNCTION_NAME)

clean:
	aws lambda delete-function --function-name $(FUNCTION_NAME)
	aws iam detach-role-policy --role-name $(ROLE_NAME) --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
	aws iam delete-role --role-name $(ROLE_NAME)

function.zip: index.js
	zip $@ $<
