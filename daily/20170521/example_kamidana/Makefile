default:
	kamidana --data ./person.yaml template.j2
	kamidana --data ./person.yaml template.j2 --dump-context
	NAME=bar AGE=10 kamidana --data ./person.yaml.env template.j2 --dump-context
	NAME=bar AGE=10 kamidana --data ./person.yaml.env template.j2
	NAME=bar kamidana --data ./person.yaml --data ./person.yaml.env template.j2 --dump-context
	NAME=bar kamidana --data ./person.yaml --data ./person.yaml.env template.j2
