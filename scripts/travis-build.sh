#!/bin/bash
sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" ]] && grep -v -q SNAPSHOT version.sbt; then
  export publish_cmd="publish"
fi

coverage="$sbt_cmd coverage validateJVM coverageReport && bash <(curl -s https://codecov.io/bash)"
scala_js="$sbt_cmd coreJS/compile" # && $sbt_cmd testsJS/test"
scala_jvm="$sbt_cmd validateJVM"

run_cmd="$coverage && $scala_js && $scala_jvm $publish_cmd"
eval $run_cmd
