```bash
$ make
python minij2.py template.json config.json
{
  "containerDefinitions": [
    {
      "command": [
        "-c",
        "./conf/dev.json"
      ],
      "cpu": 2,
      "entryPoint": [
        "./checker"
      ],
      "environment": [
        {
          "name": "SERVICE_NAME",
          "value": "dev"
        },
        {
          "name": "SERVICE_TAGS",
          "value": "analysis"
        },
        {
          "name": "ENVIRONMENT",
          "value": "dev"
        },
        {
          "name": "CLUSTER_NAME",
          "value": "checker"
        },
        {
          "name": "SERVICE_GROUP",
          "value": "analysis"
        },
        {
          "name": "TEMPLATE_GROUP",
          "value": "back"
        },
        {
          "name": "DESIRED_COUNT",
          "value": "1"
        },
        {
          "name": "MINIMUM_HEALTHY_PERCENT",
          "value": "50"
        },
        {
          "name": "MAXIMUM_PERCENT",
          "value": "200"
        }
      ],
      "essential": true,
      "image": "quay.io/me/mine:dev",
      "logConfiguration": {
        "logDriver": "syslog",
        "options": {
          "tag": "docker/dev/checker/{{.ID}}"
        }
      },
      "memoryReservation": 64,
      "name": "dev-checker",
      "portMappings": [
        {
          "containerPort": 8002,
          "hostPort": 0,
          "protocol": "tcp"
        }
      ]
    }
  ],
  "family": "dev-checker"
}
```
