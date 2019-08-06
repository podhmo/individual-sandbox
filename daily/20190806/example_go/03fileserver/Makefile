default:
	ioknife too --cmd "go run main.go" --cmd "make client"& echo $$! > x.pid && wait $$(cat x.pid)

client:
	sleep 1
	http GET :8080/go/tos.html
	pkill -TERM -g $$(ps -o pgid -p $$(cat x.pid) | tail -n 1) || echo ok

