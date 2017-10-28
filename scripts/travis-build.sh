#!/bin/bash
sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

export publish_cmd="publishLocal"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" ]] && grep -v -q SNAPSHOT version.sbt; then
  export publish_cmd="publish gitSnapshots publish"
fi

coverage="(export SCOVERAGEON=true; travis_wait $sbt_cmd coverage tests/test coverageReport && bash <(curl -s https://codecov.io/bash) )"
scala_jvm="$sbt_cmd validate"

run_cmd="$coverage && $scala_jvm $publish_cmd"
eval $run_cmd
