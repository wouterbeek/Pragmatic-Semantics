When I build a JAR package out of Steven's code [1]
 I get a JAR file that gives me error [3] upon execution [2].
A dicussion at [4] pushed me into the right direction:
 apparently the first Sesame RIO dependency is included in the JAR file,
 but the others are not.

~~~
[1] mvn clean compile assembly:single
[2] java -jar SemanticURIs-0.0.1-SNAPSHOT-jar-with-dependencies.jar
         preprocess
         ../../Data/http/dati.camera.it/ocd/files/elezione.rdf.gz/stage5
         ../../Data/http/dati.camera.it/ocd/files/elezione.rdf.gz/stage6
[3] Loading directory
    Reading '../../Data/http/dati.camera.it/ocd/files/elezione.rdf.gz/stage5/elezione.ttl'...
    Cannot load '../../Data/http/dati.camera.it/ocd/files/elezione.rdf.gz/stage5': org.openrdf.rio.UnsupportedRDFormatException: No parser factory available for RDF format Turtle (mimeTypes=text/turtle, application/x-turtle; ext=ttl)
[4] http://sourceforge.net/p/sesame/mailman/sesame-general/thread/4FC56353.7040805@gmail.com/
~~~

