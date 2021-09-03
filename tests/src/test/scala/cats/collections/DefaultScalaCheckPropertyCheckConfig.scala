package cats.collections

import org.scalacheck.Test.Parameters

object DefaultScalaCheckPropertyCheckConfig {
  final val default: Parameters = Parameters.default
    .withMinSuccessfulTests(if (BuildInfo.isJvm) 50 else 5)
    .withMaxDiscardRatio(if (BuildInfo.isJvm) 5 else 50)
    .withMinSize(0)
    .withWorkers(if (BuildInfo.isJvm) 2 else 1)
}
