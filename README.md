TransPhoner
===========

This repository contains the code for the CHI2014 paper: ["TransPhoner: Automated Mnemonic Keyword Generation"](http://bit.ly/transphoner).  The core library is implemented in Scala while a Play framework web app is used as a front end for demonstrating some simple applications.  Running the web server should be fairly simple once the below prerequisites are in place:

Prerequisites
-------------
0. Scala 2.10.3 (http://www.scala-lang.org/downloads)
1. Play 2.2.2 (http://www.playframework.com/)
2. Make sure the Play and JDK paths are added to the system path

Running the webserver
---------------------------------

To run server (on default port 9000):
```
   cd $TRANSPHONER
   play
   extractData
   run
```

Then direct your browser to http://localhost:9000 to check the server is running.  Note that compilation might take a few seconds on the first access. To clean the state of the project run `play clean` at the root directory and to debug run `play debug`.

Development
-----------
We used [IntelliJ IDEA](http://www.jetbrains.com/idea/) for development so the below instructions pertain to generating a development project for that particular IDE.  Other IDEs can be targeted by the Play framework in a similar manner.

1. Create idea project (see http://www.playframework.com/documentation/2.0.x/IDE)
```
  cd $TRANSPHONER
  play
  idea
```
2. Open project with IntelliJ IDEA

3. Debugging
 - Open Run/Debug Configurations dialog, then click Run -> Edit Configurations
   - Add a Remote configuration, then select Remote
   - Configure it:
     - Set a name
     - Transport: Socket
     - Debugger mode: Attach
     - Host: localhost
     - Port: 9999
   - Select module you imported
   - Close dialog - click Apply

Problems?
---------
- Got `sbt.ResolveException: download failed: [NOT FOUND  ] org.slf4j#slf4j-api;1.6.6!slf4j-api.jar`?
  Check `plugins.sbt` and make sure the Play version there matches your installed Play version in the line of the form: `addSbtPlugin("play" % "sbt-plugin" % "2.1.3")`

License
-------
This code is distributed under the terms of the GNU GPLv3 license.

Copyright (C) 2014 Angel X. Chang and Manolis Savva

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
