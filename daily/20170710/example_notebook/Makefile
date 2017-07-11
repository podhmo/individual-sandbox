server:
	jupyter-notebook

list:
	http GET ":8888/api/contents?type=directory"

create:
	echo '{"type": "notebook"}' | http POST :8888/api/contents

rename:
	echo '{"path": "new.ipynb"}' | http PATCH :8888/api/contents/Untitled1.ipynb

delete:
	http DELETE :8888/api/contents/new.ipynb


# sessions and kernels
specs:
	http :8888/api/kernelspecs

kernels:
	http :8888/api/kernels

create-kernel:
	echo '{"name": "python3"}' | http POST :8888/api/kernels

sessions:
	http :8888/api/sessions

create-session:
	echo '{"notebook": {"path": "Untitled.ipynb"}, "kernel": {"id": "0441ee17-6627-4ee9-89c0-1ff557ae8026", "name": "python3"}}' | http POST :8888/api/sessions
