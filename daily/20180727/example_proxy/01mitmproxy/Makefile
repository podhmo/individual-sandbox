run:
	PYTHONPATH=proxy2 python2 -W default proxy2/examples/sslstrip.py 8080
client:
	http --proxy https://localhost:8080 --cert proxy2/examples/ca.crt --cert-key proxy2/examples/ca.key https://pod.hatenablog.com/

prepare:
	git clone git@github.com:inaz2/proxy2.git

