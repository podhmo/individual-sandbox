## cqlsh

https://datastax.github.io/python-driver/installation.html

OSX

```
ARCHFLAGS=-Wno-error=unused-command-line-argument-hard-error-in-future pip install cassandra-driver
```

## cassandraのinstall

```
sudo port install cassandra
```

download

```
mvn dependency:get -DremoteRepositories=http://repo1.maven.org/maven2/ \
                   -DgroupId=junit -DartifactId=junit -Dversion=4.8.2 \
                   -Dtransitive=false
```

あるようにみえる。 https://mvnrepository.com/artifact/com.thoughtworks.paranamer/paranamer-ant/2.1


```
# Maven2 Repository Locations (you can override these in "build.properties" to point to a local proxy, e.g. Nexus)
artifact.remoteRepository.central:     http://repo1.maven.org/maven2
artifact.remoteRepository.java.net2:   http://download.java.net/maven/2
artifact.remoteRepository.apache:      https://repository.apache.org/content/repositories/releases
artifact.remoteRepository.jclouds:     http://jclouds.googlecode.com/svn/repo
artifact.remoteRepository.oauth:       http://oauth.googlecode.com/svn/code/maven
```

あるようにみえる http://repo1.maven.org/maven2/com/thoughtworks/paranamer/paranamer-ant/2.1/

これのせいじゃ？
https://trac.macports.org/ticket/30060

https://trac.macports.org/ticket/45004
https://trac.macports.org/ticket/45005

```
build.env-append        _JAVA_OPTIONS=-Duser.home=${workpath}/home
```
