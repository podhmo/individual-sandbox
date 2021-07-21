from diagrams import Diagram

# for https://aws.amazon.com/jp/getting-started/hands-on/build-web-app-s3-lambda-api-gateway-dynamodb/?sc_icampaign=acq_jp_getting-started-handson-202010-build-web-app-s3-lambda-api-gateway-dynamodb&sc_language=jp&sc_icontent=awssm-6336&sc_iplace=ribbon&trk=ha_ribbon_acq_jp_getting-started-handson-202010-build-web-app-s3-lambda-api-gateway-dynamodb
# doc https://diagrams.mingrammer.com/docs/nodes/aws
from diagrams.aws.mobile import Amplify
from diagrams.aws.mobile import APIGateway
from diagrams.aws.compute import Lambda
from diagrams.aws.security import IAM
from diagrams.aws.database import Dynamodb

# amplify, api gateway, lambda, aws identity and access management, dynamodb

# with Diagram("architecture", show=False, direction="TB"):    
with Diagram("architecture", show=True, direction="LR", outformat="svg"):
    amplify = Amplify("AWS Amplify") 
    api_gateway = APIGateway("Amazon API Gateway")
    aws_lambda = Lambda("AWS lambda")
    iam = IAM("AWS Identity and Access Management")
    dynamo_db = Dynamodb("Amazon DynamoDB")

    amplify >> api_gateway >> aws_lambda >> dynamo_db
    iam >> aws_lambda
    