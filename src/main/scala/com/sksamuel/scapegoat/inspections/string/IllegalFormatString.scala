package com.sksamuel.scapegoat.inspections.string

import java.util.IllegalFormatException

import com.sksamuel.scapegoat._

/** @author Stephen Samuel */
class IllegalFormatString extends Inspection("Illegal format string", Levels.Error) {

  // format is: %[argument_index$][flags][width][.precision][t]conversion
  final val argRegex = "%(\\d+\\$)?[-#+ 0,(\\<]*?\\d*(\\.\\d+)?[tT]?[a-zA-Z]".r

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(Apply(Select(_, TermName("augmentString")), List(Literal(Constant(format)))),
            TermName("format")), _) =>
            val argCount = argRegex.findAllIn(format.toString).matchData.size
            warning(s">>>>>>>>>>>>> Arg count $argCount")
            val args = Nil.padTo(argCount, null)
            warning(s">>>>>>>>>>>>> Args $args")
            warning(s">>>>>>>>>>>>> Format ${format.toString}")
            try {
              String.format(format.toString, args: _*)
            } catch {
              case e: IllegalFormatException =>
                println(e)
                context.warn(tree.pos, self, "A format string contains an illegal syntax: " + e.getMessage)
            }
          case _ => continue(tree)
        }
      }
    }
  }
}
