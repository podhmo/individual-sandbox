run:
	sbt compile run

runJarMac:
	sbt compile
	`port contents scala2.10 | grep bin/scala-2.10` target/scala-2.10/hello_2.10-1.0.jar
