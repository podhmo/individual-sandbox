TYPE  ?= svg

default:
	modulegraph --outfile /tmp/a.dot dictknife concat a.json --dst b.json
	cat /tmp/a.dot | sed "s@$$HOME@~@g" > a.dot
	dot -T${TYPE} a.dot > a.${TYPE}

clean:
	rm -f *.${TYPE} *.dot
