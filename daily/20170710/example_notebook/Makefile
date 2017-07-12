TOKEN=token==1eb30c9d12d500cd82f1d837d178d5bd88a96cf10c88f43f

server:
	jupyter-notebook --no-browser

setup:
	pip install jupyter httpie
	go get github.com/raphael/wsc

list:
	http GET :8888/api/contents type==directory ${TOKEN}

create:
	echo '{"type": "notebook"}' | http POST :8888/api/contents ${TOKEN}

rename:
	echo '{"path": "new.ipynb"}' | http PATCH :8888/api/contents/Untitled.ipynb ${TOKEN}

delete:
	http DELETE :8888/api/contents/new.ipynb


# sessions and kernels
specs:
	http :8888/api/kernelspecs ${TOKEN}

kernels:
	http :8888/api/kernels ${TOKEN}

create-kernel:
	echo '{"name": "python3"}' | http POST :8888/api/kernels ${TOKEN} | tee kernel.json

sessions:
	http :8888/api/sessions ${TOKEN}

create-session:
	http --verbose POST :8888/api/sessions ${TOKEN} kernel:=@kernel.json notebook:='{"path": "Untitled.ipynb"}' | tee session.json

# # TODO
# ws-connect:
# 	wsc ws://localhost:8888/	

