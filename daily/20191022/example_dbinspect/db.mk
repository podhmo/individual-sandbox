HONY: help start stop mysql exec run

# 引数がないときはusageを表示する
.DEFAULT_GOAL := help

start: ## Start MySQL Conatainer
	docker container run --rm -d -e MYSQL_ALLOW_EMPTY_PASSWORD=yes \
		-p 43306:3306 --name mysql_til budougumi0617/mysql-sakila:8.0

stop: ## Terminate MySQL Container
	docker container stop mysql_til

mysql: ## Connect to MySQL
	mysql -h 127.0.0.1 --port 43306 -uroot -D sakila

CMD=show tables;
exec: ## Execute query on MySQL ex: make exec CMD="show columns from country"
	mysql -h 127.0.0.1 --port 43306 -uroot -D sakila -e "${CMD}"

FILE=''
run: ## Run quey from file on MySQL ex: make run FILE=./count_city.sql
	mysql -h 127.0.0.1 --port 43306 -uroot -D sakila < ${FILE}

# 各コマンドについたコメントを表示する
help: ## Show options
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'
