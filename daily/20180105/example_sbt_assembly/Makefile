run:
	sbt clean assembly
	java -jar target/scala-2.10/hello-assembly-1.0.jar

runWithDeps:
	sbt clean assemblyPackageDependency package
	java -cp "target/scala-2.10/hello-assembly-1.0-deps.jar:target/scala-2.10/hello_2.10-1.0.jar" foo.bar.baz.Main
