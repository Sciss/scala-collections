## Readme

This is basically Daniel Spiewak's [scala-collections](https://github.com/djspiewak/scala-collections) project, merely updated to build with sbt and against Scala 2.9.1. Some of the data structures in turn are build on work by Rich Hickey, and are provided under a BSD style license.

Thus, to build: `sbt compile`.

### creating an IntelliJ IDEA project

To develop the sources with IntelliJ IDEA, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "collection"
    > gen-idea
