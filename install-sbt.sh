#!/bin/sh

mkdir -p $HOME/.local/bin/
curl -s https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt > $HOME/.local/bin/sbt
chmod 0755 $HOME/.local/bin/sbt
