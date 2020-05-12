default:
	python main.py | tee result.proto
	diff -u expected.proto result.proto > a.diff || exit 0

doc:
	protoc --doc_out=html,index.html:./ result.proto
# gen:
# 	protoc --go_out=plugins=grpc:. helloworld.proto
