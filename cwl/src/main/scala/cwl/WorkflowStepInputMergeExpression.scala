package cwl

import cats.syntax.validated._
import cwl.command.ParentName
import wom.expression.IoFunctionSet
import wom.graph.GraphNodePort.OutputPort
import wom.types.WomType
import wom.values.{WomFile, WomValue}

class WorkflowStepInputMergeExpression(input: WorkflowStepInput,
                                       override val cwlExpressionType: WomType,
                                       // cats doesn't have NonEmptyMap (yet https://github.com/typelevel/cats/pull/2141/)
                                       // This is an ugly way to guarantee this class is only instantiated with at least one mapping
                                       stepInputMappingHead: (String, OutputPort),
                                       stepInputMappings: Map[String, OutputPort],
                                       override val expressionLib: ExpressionLib)(implicit parentName: ParentName) extends CwlWomExpression {
  private val allStepInputMappings = stepInputMappings + stepInputMappingHead

  override def sourceString = s"${input.id}-Merge-Expression"
  override def inputs = allStepInputMappings.keySet
  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    if (allStepInputMappings.size > 1) {
      // TODO add Dan's logic in here (this class can be changed to accomodate that)
      "MultipleStepInputRequirement not supported yet".invalidNel
    } else {
      val (inputName, _) = allStepInputMappings.head
      inputValues(inputName).validNel
    }
  }
  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = {
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
