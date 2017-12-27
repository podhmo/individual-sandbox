serve:
	python -m reqtrace.commands.echo --port 44444 --pretty

hello:
	http :44444/hello

hello2:
	http :44444/hello name==foo age==20 skills==x skills==y skills==z

hello3:
	http -a admin:admin :44444/auth

send:
	http --form POST :44444/people name=foo age=20

send2:
	echo '{"name": "foo", "age": 20}' | http --json POST :44444/people
