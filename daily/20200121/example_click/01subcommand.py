# from https://dev.classmethod.jp/tool/python-library-click/
import click
import boto3
import json


@click.group(help="Subcommand click CLI")  # (1)
@click.option("-p", "--profile", type=str)
@click.pass_context
def main(ctx, profile):
    ctx.params["session"] = boto3.session.Session(
        profile_name=ctx.params.get("profile")
    )


@main.group(help="EC2 API")  # (2)
@click.pass_context
def ec2(ctx):
    ctx.params["client"] = ctx.parent.params["session"].client("ec2")


@main.group(help="S3 API")
@click.pass_context
def s3(ctx):
    ctx.params["client"] = ctx.parent.params["session"].client("s3")


@main.group(help="RDS API")
@click.pass_context
def rds(ctx):
    ctx.params["client"] = ctx.parent.params["session"].client("rds")


@ec2.command(help="EC2 DescribeInstances API")
@click.option("--instance-id", type=str, help="specify instance id")
@click.pass_context
def describe_instances(ctx, instance_id):
    instance_ids = [instance_id] if instance_id else []
    print(
        json.dumps(
            ctx.parent.params["client"].describe_instances(InstanceIds=instance_ids)
        )
    )


@ec2.command(help="EC2 RunInstances API")
def run_instances(ctx):
    pass


if __name__ == "__main__":
    main()
