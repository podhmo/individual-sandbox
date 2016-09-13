# npm npmでsudo状態でprivate repositoryをinstall

```
GIT_SSH_COMMAND='ssh -i ~/.ssh/id_rsa' sudo -E npm install -g "git+ssh://git@github.com:<user>/repository"
```

# mgo

- https://gist.github.com/border/3489566
