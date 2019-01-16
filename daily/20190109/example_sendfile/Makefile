FILE := /tmp/xxx.dummy
OUTPUT ?= output.csv

client:
	(time python client.py)

sampling:
	echo real,user,sys > ${OUTPUT}
	for i in `seq 10`; do (time python client.py) &>> ${OUTPUT}; done

server:
	python server.py ${FILE}
server2:
	python server2.py ${FILE}

gen: clean
	fallocate -l 1GB ${FILE}
	ls -lh ${FILE}
gen2: clean
	fallocate -l 10MB ${FILE}
	ls -lh ${FILE}

clean:
	rm -f ${FILE}
