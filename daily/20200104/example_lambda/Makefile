AWS := aws --profile=lambda
NAME ?= my-function

list:
	${AWS} lambda list-functions


# https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/gettingstarted-awscli.html
create-00:
	zip function.zip index.js

create-01: ROLE ?= $(shell jqfpy 'get("Role/Arn")' role.json -r)
create-01:
	${AWS} lambda create-function --function-name ${NAME} --zip-file fileb://function.zip --handler index.handler --runtime nodejs12.x --role ${ROLE}

run-00:
	${AWS} lambda invoke --function-name ${NAME} out --log-type Tail | tee run.output
run-01:
	cat run.output | jqfpy 'get("LogResult")' -r | python -c 'import base64; import sys; base64.decode(sys.stdin.buffer, sys.stdout.buffer)'

get-00:
	${AWS} lambda get-function --function-name ${NAME}

list-role:
	aws iam list-roles
get-role:
	aws iam get-role --role-name lambda-cli-role | tee role.json

clean:
	${AWS} lambda delete-function --function-name ${NAME}
