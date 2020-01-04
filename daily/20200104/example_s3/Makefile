NAME := hello-hello-hello

## menu
# - create bucket
# - activate versioning
# - TODO: activate sqs hook

00:
	aws s3api list-buckets

# create
01:
	# acl is private, public-read, public-read-write, authenticated-read
	aws s3api create-bucket --acl private --bucket ${NAME} --region ap-northeast-1  --create-bucket-configuration LocationConstraint=ap-northeast-1
# acl
02:
	aws s3api get-bucket-acl --bucket ${NAME}

# check versioning
03:
	aws s3api get-bucket-versioning --bucket ${NAME}
# activate versioning
04:
	aws s3api put-bucket-versioning --bucket ${NAME} --versioning-configuration Status=Enabled

# ls
05:
	aws s3 ls s3://${NAME}
# cp https://hello-hello-hello.s3-ap-northeast-1.amazonaws.com/hello.txt
06:
	aws s3 cp hello.txt s3://${NAME}/

# cp -> local
07:
	aws s3 cp s3://${NAME}/hello.txt downloaded.txt

08:
	aws s3api list-object-versions --bucket ${NAME}
	# aws s3api get-object --bucket ${NAME} --key hello.txt --version-id # xxxxXxxxXxXxXxXxXxXxxxxXXxxxxXXx /tmp/hoge.txt

# public?
09:
	aws s3api get-bucket-policy-status --bucket ${NAME}

# 署名付きURL (expires 300s)
10:
	aws s3 presign s3://${NAME}/hello.txt --expires-in 300
# e.g.
# https://hello-hello-hello.s3.amazonaws.com/hello.txt?AWSAccessKeyId=AKIA3NHN6ZQQG56YO3F7&Signature=xdq54q7BtnqOq9ZZdxsJP0qbryA%3D&Expires=1578162385

# rm
11:
	aws s3 rm s3://${NAME}/hello.txt


clean:
	python clean.py --bucket ${NAME}
# aws s3 rm s3://${NAME} --recursive
# aws s3 rb s3://${NAME} # --force  
# remove_bucket failed: s3://hello-hello-hello An error occurred (BucketNotEmpty) when calling the DeleteBucket operation: The bucket you tried to delete is not empty. You must delete all versions in the bucket.


user:
#	aws iam get-account-authorization-details
	aws iam get-user
	# aws iam get-user-policy --user-name monogusa

