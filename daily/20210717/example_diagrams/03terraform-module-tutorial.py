import typing as t


class Var:
    vpc_name: str
    vpc_cidr: str
    vpc_azs: str
    vpc_public_subnets: t.List[str]
    vpc_private_subnets: t.List[str]

class Output:
    xxx: str

def define_module(
    var: Var,
) -> Output:
    # provider setup
    terraform(required_providers(aws(source="hashicorp/aws")))
    aws = provider(region="us-west-2")

    vpc_module = module(
        source="terraform-aws-modules/vpc/aws",
        version="2.21.0",
        name=var.vpc_name,
        cidr=var.vpc_cidr,
        azs=var.vpc_azs,
        private_subnets=var.vpc_private_subnets,
        public_subnets=var.vpc_public_subnets,
        enable_nat_gateway=var.vpc_enable_nat_gateway,
        tags=var.vpc_tags,
    )

    ec2_instances_module = module(
        source="terraform-aws-modules/ec2-instance/aws",
        version="2.12.0",
        name="my-ec2-cluster",
        instance_count=2,
        ami="ami-0c5204531f799e0c6",
        instance_type="t2.micro",
        vpc_security_group_ids=[vpc_module.default_security_group_id],
        subnet_id=vpc_module.public_subnets[0],
        tags={
            "Terraform": "true",
            "Environment": "dev",
        },
    )

    return Output(xxx=vpc_module.this.xxx)
