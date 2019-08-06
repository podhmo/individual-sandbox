default:
	ioknife too --cmd "make server" --cmd "make client"

server:
	go run main.go& echo $$! > x.pid && wait -p $$(cat x.pid)

client:
	sleep 1
	http --timeout 1 GET :8080/go/tos.html
	pkill -TERM -g $$(cat x.pid)
# broken
