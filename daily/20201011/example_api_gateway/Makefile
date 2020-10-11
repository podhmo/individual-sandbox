import:
	aws apigatewayv2 import-api --body file://api-definition.json

billing:
	aws ce get-cost-and-usage --output table --time-period Start=$$(gdate '+%Y-%m')-01,End=$$(gdate -d 'next month' '+%Y-%m')-01 --metrics BlendedCost --granularity MONTHLY --group-by Type=DIMENSION,Key=SERVICE
