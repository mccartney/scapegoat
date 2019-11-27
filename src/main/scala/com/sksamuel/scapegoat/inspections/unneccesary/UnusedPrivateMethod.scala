package com.sksamuel.scapegoat.inspections.unneccesary

import com.sksamuel.scapegoat._

import scala.collection.mutable.{Set => MutableSet}

class UnusedPrivateMethod extends Inspection("Unused private method", Levels.Warning) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._
      import definitions._

      private val methodsNotSeenYet = MutableSet[String]()

      override final def inspect(tree: Tree): Unit = {
        tree match {
          case ClassDef(_, _, _, template) =>
            val methodsDeclaredAtThisLevel = template.children.collect {
              case DefDef(mods, TermName(name), _, _, _, _) if mods.hasFlag(Flag.PRIVATE) =>
                name
            }
            methodsNotSeenYet ++= methodsDeclaredAtThisLevel
            continue(tree)
            methodsNotSeenYet.foreach { name =>
              context.warn(tree.pos, self, s"Dead code - private method $name not used anywhere: " + tree.toString().take(200))
            }
            methodsNotSeenYet --= methodsDeclaredAtThisLevel

          case Ident(TermName(name)) =>
            methodsNotSeenYet -= name
            continue(tree)

          case _ => continue(tree)
        }
      }
    }
  }
}
