package cromwell.backend.impl.jes

import cromwell.backend.standard.{StandardExpressionFunctions, StandardExpressionFunctionsParams}
import cromwell.core.io.IoCommandBuilder
import cromwell.filesystems.gcs.GcsPathBuilder
import cromwell.filesystems.gcs.GcsPathBuilder.{InvalidGcsPath, PossiblyValidRelativeGcsPath, ValidFullGcsPath}
import cromwell.filesystems.gcs.batch.GcsBatchCommandBuilder

class JesExpressionFunctions(standardParams: StandardExpressionFunctionsParams)
  extends StandardExpressionFunctions(standardParams) {
  override lazy val ioCommandBuilder: IoCommandBuilder = GcsBatchCommandBuilder

  override def preMapping(str: String) = {
    GcsPathBuilder.validateGcsPath(str) match {
      case _: ValidFullGcsPath => str
      case PossiblyValidRelativeGcsPath => callContext.root.resolve(str.stripPrefix("/")).pathAsString
      case invalid: InvalidGcsPath => throw new IllegalArgumentException(invalid.errorMessage)
    }
  }
}
