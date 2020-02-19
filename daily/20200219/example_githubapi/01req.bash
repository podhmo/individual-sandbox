echo '{"query": "query { viewer { login }}"}' | http --json POST https://api.github.com/graphql "Authorization":"bearer ${TOKEN}"
