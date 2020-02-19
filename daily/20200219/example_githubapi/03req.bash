echo '{"query": "query { 
  repository(owner: \"podhmo\", name: \"prestring\") { 
    pullRequests(states: [OPEN], last:100){
      nodes{
        title
        baseRefName
      }
    }
  }
}
"}' | http --json POST https://api.github.com/graphql "Authorization":"bearer ${TOKEN}"

