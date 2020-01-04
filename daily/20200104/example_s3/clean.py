from handofcats import as_command
import boto3

# on awscli the error is raised, following:
# remove_bucket failed: s3://hello-hello-hello An error occurred (BucketNotEmpty) when calling the DeleteBucket operation: The bucket you tried to delete is not empty. You must delete all versions in the bucket.


@as_command
def run(*, bucket: str) -> None:
    s3 = boto3.resource("s3")
    bucket = s3.Bucket(bucket)
    print(bucket.object_versions.delete())

    # if you want to delete the now-empty bucket as well, uncomment this line:
    print(bucket.delete())
