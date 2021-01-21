## metashape 参照を使った記述について考えてみる

- https://github.com/podhmo/metashape/issues/98

参考になるのはterraform?

### terraformの値の考え方

- https://www.terraform.io/docs/configuration/variables.html
- https://www.terraform.io/docs/configuration/outputs.html
- https://learn.hashicorp.com/tutorials/terraform/outputs
- https://learn.hashicorp.com/tutorials/terraform/variables

### variable

description, default, type

```
variable "vpc_cidr_block" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}
```

with

```
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "2.44.0"

-  cidr = "10.0.0.0/16"
+  cidr = var.vpc_cidr_block
# ...
}
```

### aws cdk

- https://github.com/aws-samples/aws-cdk-examples/tree/master/python/api-sqs-lambda
- https://docs.aws.amazon.com/cdk/api/latest/python/index.html
- https://github.com/aws-samples/aws-cdk-examples/tree/master/python/api-sqs-lambda

概念的な話

- App, Stack, Construct
- High-level constructs, Low-level construcs

かなり強引？

```
export class FargateStack extends cdk.Stack {
  ...
  const cluster = ssm.StringParameter.fromStringParameterAttributes(
    this, 'SSMUser', {
      parameterName: 'MySystem/Dev/DB/User'
    }
  )
}
```


依存関係

```
const app = new cdk.App();
const vpc_stack = new VPCStack(app, 'VPCStack');
const alb_stack = new ALBStack(app, 'ALBStack', vpc_stack.vpc);
alb_stack.addDependency(vpc_stack);
```

- https://techlog.voyagegroup.com/entry/protoc-gen-swagger


### prt

- https://github.com/nytimes/openapi2proto
- https://github.com/grpc-ecosystem/grpc-gateway/tree/master/protoc-gen-openapiv2
