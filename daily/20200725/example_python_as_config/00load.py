import shape
import json
import dataclasses

config = shape.Config(
    metadata={"name": "deployment-example"},
    spec=shape.ConfigSpec(
        replicas=3,  # replicasは3未満に制限
        template=shape.ConfigTemplate(
            metadata={"labels": {"app": "deployment-example"}},
            spec=shape.ConfigTemplateSpec(
                containers=[
                    shape.k8shape.Container(
                        name="sample",
                        image="sample",  # docker imageのtagの省略禁止
                        ports=[shape.k8shape.PortSetting(containerPort=8)],
                    )
                ]
            ),
        ),
    ),
)

print(config)
print(json.dumps(dataclasses.asdict(config), indent=2, ensure_ascii=False))
