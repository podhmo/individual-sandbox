## go go-playground/validator

- enumはどうする？
- validation errorをいい感じにしたくない？
- (aws-lambda-go/event に合わせたくない？)
- list,mapの子要素に反映するには > dive

### 使えそうなもの

https://pkg.go.dev/github.com/go-playground/validator

- required
- dive
- gt,gte,lt,lte
- oneof
- contains,containsany,exclude,excludesall
- startswith,endswith
- regex は `|` が死ぬので使わないほうが良い


## inflexible

- lambda + api gatwayでdeployしたい
- workerとかどうなるんだろう？


## go `:=` がだるい


