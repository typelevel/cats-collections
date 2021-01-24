#!/bin/bash
sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" ]] && grep -v -q SNAPSHOT version.sbt; then
  export publish_cmd="publish gitSnapshots"
fi

run_cmd="$sbt_cmd validate $publish_cmd"
eval $run_cmd
