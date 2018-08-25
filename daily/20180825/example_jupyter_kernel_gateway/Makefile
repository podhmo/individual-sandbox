serve:
	# jupyter kernelgateway --port=9001 --KernelGatewayApp.api=notebook-http --KernelGatewayApp.ip=0.0.0.0 --KernelGatewayApp.seed_uri=kernel_gateway_demos/scotch_demo/notebooks/scotch_api_python.ipynb
	jupyter kernelgateway --debug --port=9001 --KernelGatewayApp.api=notebook-http --KernelGatewayApp.ip=0.0.0.0 --KernelGatewayApp.seed_uri=nums.ipynb

client:
	http :9001/nums

serve2:
	jupyter kernelgateway --debug --port=9001 --KernelGatewayApp.api=notebook-http --KernelGatewayApp.ip=0.0.0.0 --KernelGatewayApp.seed_uri=https://raw.githubusercontent.com/jupyter/kernel_gateway/master/etc/api_examples/api_intro.ipynb

client2:
	http :9001/nums

setup:
	pip install jupyter-kernel-gateway
	git clone --depth=1 git@github.com:jupyter/kernel_gateway_demos

notebook:
	jupyter notebook --ip=0.0.0.0
