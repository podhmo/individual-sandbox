## aws cdk

- https://aws.amazon.com/jp/blogs/developer/getting-started-with-the-aws-cloud-development-kit-and-python/
- https://docs.aws.amazon.com/cdk/latest/guide/work-with-cdk-python.html

hmm。ここから読むのが正しそう。

- https://docs.aws.amazon.com/cdk/latest/guide/getting_started.html

### layer?

- L1 AWS CloudFformation only
- L2 Cuarted
- L3 Patterns

何言っているかわかんないな？

### 利用できる言語

Python,TypeScript,JavaScript,Java,C#

### pre-requirements

どんな言語でもnode.jsの10.3.0以降が必要？

```console
$ npm install -g aws-cdk
$ cdk --version
```

### 実際の操作

- https://docs.aws.amazon.com/cdk/latest/guide/hello_world.html

```
# ./node_modules/.bin/cdk init TEMPLATE --language LANGUAGE
$ mkdir app; cd app
./node_modules/.bin/cdk init app --language python
```

使えそうテンプレートを見る。

```
Available templates:
* app: Template for a CDK Application
   └─ cdk init app --language=[csharp|fsharp|java|javascript|python|typescript]
* lib: Template for a CDK Construct Library
   └─ cdk init lib --language=typescript
* sample-app: Example CDK Application with some constructs
   └─ cdk init sample-app --language=[csharp|fsharp|java|javascript|python|typescript]

```

使えそうなオプションの表示をしてくれていた


* cdk ls           list all stacks in the app
* cdk synth        emits the synthesized CloudFormation template
* cdk deploy       deploy this stack to your default AWS account/region
* cdk diff        compare deployed stack with current state
* cdk docs        open CDK documentation

### cdk synth

こんな結果を返す

```yaml
Resources:
  CDKMetadata:
    Type: AWS::CDK::Metadata
    Properties:
      Modules: aws-cdk=1.74.0,@aws-cdk/cloud-assembly-schema=1.74.0,@aws-cdk/core=1.74.0,@aws-cdk/cx-api=1.74.0,@aws-cdk/region-info=1.74.0,jsii-runtime=Python/3.8.5
    Metadata:
      aws:cdk:path: app/CDKMetadata/Default
    Condition: CDKMetadataAvailable
Conditions:
  CDKMetadataAvailable:
    Fn::Or:
      - Fn::Or:
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-east-1
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-northeast-1
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-northeast-2
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-south-1
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-southeast-1
          - Fn::Equals:
              - Ref: AWS::Region
              - ap-southeast-2
          - Fn::Equals:
              - Ref: AWS::Region
              - ca-central-1
          - Fn::Equals:
              - Ref: AWS::Region
              - cn-north-1
          - Fn::Equals:
              - Ref: AWS::Region
              - cn-northwest-1
          - Fn::Equals:
              - Ref: AWS::Region
              - eu-central-1
      - Fn::Or:
          - Fn::Equals:
              - Ref: AWS::Region
              - eu-north-1
          - Fn::Equals:
              - Ref: AWS::Region
              - eu-west-1
          - Fn::Equals:
              - Ref: AWS::Region
              - eu-west-2
          - Fn::Equals:
              - Ref: AWS::Region
              - eu-west-3
          - Fn::Equals:
              - Ref: AWS::Region
              - me-south-1
          - Fn::Equals:
              - Ref: AWS::Region
              - sa-east-1
          - Fn::Equals:
              - Ref: AWS::Region
              - us-east-1
          - Fn::Equals:
              - Ref: AWS::Region
              - us-east-2
          - Fn::Equals:
              - Ref: AWS::Region
              - us-west-1
          - Fn::Equals:
              - Ref: AWS::Region
              - us-west-2

```

## この辺を見ても良いのかも？

- https://dev.classmethod.jp/articles/trying-aws-cdk-tutorial-with-typescript/
- https://dev.classmethod.jp/articles/one-liner-that-set-temporary-credential-to-environment-variable-acquired-by-switch-roll/

### cdk terraform

cdktfを使う？

- https://learn.hashicorp.com/tutorials/terraform/cdktf-build

```
npm install -g cdktf-cli@next
```

あ、古いterraformじゃむりか。あとterraform cliもいるの？

```console
sudo port install terraform-0.13 terraform_select
```

## hmm? npx でうまくいかない

なんか色々と混ざっているっぽい？

```
$ npx cdk init
npm ERR! cb.apply is not a function

npm ERR! A complete log of this run can be found in:
npm ERR!     /Users/nao/.npm/_logs/2020-11-23T19_45_21_880Z-debug.log
Install for [ 'cdk@latest' ] failed with code 1
(my) [04:45:22] ~/vboxshare/venvs/my/individual-sandbox/daily/20201124/example_cdk/00hello-cdk $ npx cdk init --help
npm ERR! cb.apply is not a function

npm ERR! A complete log of this run can be found in:
npm ERR!     /Users/nao/.npm/_logs/2020-11-23T19_45_27_680Z-debug.log
Install for [ 'cdk@latest' ] failed with code 1
```

- https://github.com/nodejs/help/issues/2874

```console
$ rm ~/.nvm/.cache
$ npm cache clean --force
```
