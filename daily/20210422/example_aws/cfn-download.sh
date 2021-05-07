#!/bin/bash

# https://dev.classmethod.jp/articles/cloudformation-template-and-parameter-download-by-aws-cli/

DATE=`date +%Y%m%d_%H-%M-%S`

# StackName List Create
aws cloudformation describe-stacks --query 'Stacks[].StackName[]' | egrep -v '\[|\]' | sed 's/    "//g' | sed 's/"//g' | sed 's/,//g' > StackName-list-${DATE}.txt

# Cfn Template & Parameter Download
cat StackName-list-${DATE}.txt | while read LINE
 do
  aws cloudformation get-template --stack-name ${LINE} --query 'TemplateBody' > ${LINE}_${DATE}.template
  aws cloudformation describe-stacks --stack-name ${LINE} --output table > ${LINE}-parameter_${DATE}.txt
done

