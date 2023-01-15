import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.tools.mima.core._

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  // Methods and other things defined as `private [collections]` and therefore
  // not supposed to be referenced externally.
  exclude[IncompatibleMethTypeProblem]("cats.collections.Diet.splitMax"),
  // PairingHeap#Leaf has always been private so it's unclear why Mima gets triggered on its changes.
  exclude[MissingTypesProblem]("cats.collections.PairingHeap$Leaf$"),
  exclude[Problem]("cats.collections.PairingHeap#Leaf.*")
)
