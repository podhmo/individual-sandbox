## policy as code?

https://www.hashicorp.com/blog/why-policy-as-code

```json
"mock": {
    "tfplan": {
      "variables": {
        "env": "production"
      },
      "config": {
        "providers": {
            "aws": {
              "alias": [
                {"config": {"region": "us-west-1"}}
              ]
            }
        }
      }
    }
  },

  "test": {
      "main": true
  }
}
```

### できるvalidation

ある値に依存したenum(valid region)

- stagingのときはus-east-1
- productionのときはus-west-1

述語の定義(`is_not_aws(type)` typeが "aws" ではない)

```
import "tfplan"

valid_regions = {"production": "us-west-1", "staging": "us-east-1"}
env_region = valid_regions[tfplan.variables.env]

is_not_aws = func(type) {
    return type is not "aws"
}

// Check the provider alias region matches the environment region
validate_aws_region = func(provider) {
    return all provider.alias as a {
        a.config.region is env_region
    }
}

main = rule {
  all tfplan.config.providers as type, provider {
    is_not_aws(type) or validate_aws_region(provider)
  }
}
```
