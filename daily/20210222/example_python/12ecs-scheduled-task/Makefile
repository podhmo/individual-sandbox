00:
	docker build -t scheduled-ecs-task .
	docker run scheduled-ecs-task

01:
	eval “$(aws ecr get-login — no-include-email — region us-west-2)”
	docker build -t scheduled-ecs .
	docker tag scheduled-ecs:latest “YOUR_ECR_REPO_URI:latest”
	docker push “YOUR_ECR_REPO_URI:latest”

