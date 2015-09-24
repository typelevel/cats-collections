#!/bin/bash
sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

coverage="$sbt_cmd coverage test coverageReport && bash <(curl -s https://codecov.io/bash)"

eval $coverage
