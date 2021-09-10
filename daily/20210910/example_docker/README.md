aa

```console
$ docker run --rm elasticsearch:7.14.1 tar -czf - /usr/share/elasticsearch/config > config.tar.gz
tar: Removing leading `/' from member names

$ tar -tf config.tar.gz
usr/share/elasticsearch/config/
usr/share/elasticsearch/config/users_roles
usr/share/elasticsearch/config/jvm.options
usr/share/elasticsearch/config/roles.yml
usr/share/elasticsearch/config/role_mapping.yml
usr/share/elasticsearch/config/elasticsearch.yml
usr/share/elasticsearch/config/log4j2.file.properties
usr/share/elasticsearch/config/log4j2.properties
usr/share/elasticsearch/config/users
usr/share/elasticsearch/config/jvm.options.d/
```
