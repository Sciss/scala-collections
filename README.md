## FingerTree

The original finger tree class is in package `com.codecommit`. It was written by [Daniel Spiewak](https://github.com/djspiewak/scala-collections). These classes have been updated to build with sbt and against Scala 2.9.1, and continue to be covered by a BSD style license.

The new work, optimised, corrected and enhanced finger trees providing measurements for indexing, ordering, and summing, are (C)opright 2011&ndash;2012 by Hanns Holger Rutz, and licensed under the GNU General Public License.

### Building

Simple: `sbt compile`

### creating an IntelliJ IDEA project

To develop the sources with IntelliJ IDEA, if you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "FingerTree"
    > gen-idea
