default: 00

00:
	modulegraph --outfile=00time.dot swagger-marshmallow-codegen swagger.yaml
	modulegraph --metadata=memory --outfile=00memory.dot swagger-marshmallow-codegen swagger.yaml
	dot -Tsvg 00time.dot > 00time.svg
	dot -Tsvg 00memory.dot > 00memory.svg
