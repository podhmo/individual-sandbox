echo '{"query": "query { organization(login:\"podhmo-sandbox\") { createdAt name }}"}' | http --json POST https://api.github.com/graphql "Authorization":"bearer ${TOKEN}"
