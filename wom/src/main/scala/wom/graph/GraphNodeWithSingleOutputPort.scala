package wom.graph

import wom.graph.GraphNodePort.OutputPort

trait GraphNodeWithSingleOutputPort extends GraphNode {
  def singleOutputPort: OutputPort
}
