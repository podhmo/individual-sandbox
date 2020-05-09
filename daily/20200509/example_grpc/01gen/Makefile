default:
	python main.py | tee result.proto

doc:
	protoc --doc_out=html,index.html:./ result.proto
# gen:
# 	protoc --go_out=plugins=grpc:. helloworld.proto
