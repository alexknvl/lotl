First you will need to install SBT (Scala Build Tool), unless you already
have it. To install, simply run:

    ./install-sbt.sh

Then, if your $PATH does not include `~/.local/bin`, run:

    export PATH=${PATH}:~/.local/bin/

At this point you can run `sbt run` to start the "interpreter" (this may
take a while because SBT will be downloading its libraries and plugins):

    sbt run

If you want, you can make it read a file `input.txt` by running:

    sbt "run input.txt"

Or make a `.jar` file by running:

    sbt assembly
    cp target/scala-2.11/lotl-assembly-0.1-SNAPSHOT.jar lotl.jar

And then run this `lotl.jar` file:

    java -jar lotl.jar
