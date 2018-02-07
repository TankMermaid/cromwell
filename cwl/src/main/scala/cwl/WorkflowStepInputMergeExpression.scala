package cwl

import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import cwl.command.ParentName
import wom.expression.IoFunctionSet
import wom.graph.GraphNodePort.OutputPort
import wom.types.WomType
import wom.values.{WomFile, WomValue}

final case class WorkflowStepInputMergeExpression(input: WorkflowStepInput,
                                       // cats doesn't have NonEmptyMap (yet https://github.com/typelevel/cats/pull/2141/)
                                       // This is an ugly way to guarantee this class is only instantiated with at least one mapping
                                       stepInputMappingHead: (String, OutputPort),
                                       stepInputMappings: Map[String, OutputPort],
                                       override val expressionLib: ExpressionLib) extends CwlWomExpression {
  private val allStepInputMappings = stepInputMappings + stepInputMappingHead
  // TODO add Dan's logic in here (this class can be changed to accomodate that)
  override val cwlExpressionType = stepInputMappingHead._2.womType

  override def sourceString: String = s"${input.id}-Merge-Expression"
  override def inputs: Set[String] = allStepInputMappings.keySet
  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    if (allStepInputMappings.size > 1) {
      // TODO add Dan's logic in here (this class can be changed to accomodate that)
      "MultipleStepInputRequirement not supported yet".invalidNel
    } else {
      val (inputName, _) = allStepInputMappings.head
      inputValues(inputName).validNel
    }
  }
  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] = {
    if (allStepInputMappings.size > 1) {
      // TODO add Dan's logic in here (this class can be changed to accomodate that)
      "MultipleStepInputRequirement not supported yet".invalidNel
    } else {
      val (inputName, _) = allStepInputMappings.head
      inputTypes(inputName).collectAsSeq({
        case file: WomFile => file
      }).toSet.validNel
    }
  }
}
