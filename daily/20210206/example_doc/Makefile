default:
	python docgen.py shapes.py > 01README.md
	python docgen.py ecs-shapes.py > ECS-README.md

# default: ecs-shapes.py

ecs-shapes.py: ecs-shrink-openapi.json
	python -m metashape.inputs.openapi $< > $@

ecs-shrink-openapi.json: ecs-openapi.json
	python shrink.py ecs-openapi.json --output $@
