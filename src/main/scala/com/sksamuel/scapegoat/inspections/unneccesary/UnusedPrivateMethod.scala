package com.sksamuel.scapegoat.inspections.unneccesary

import com.sksamuel.scapegoat._

import scala.reflect.internal.ModifierFlags
import scala.collection.mutable.{Map => MutableMap}

class UnusedPrivateMethod extends Inspection("Unused private method", Levels.Warning) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._
      import definitions._

      private val methodsNotSeenYet = MutableMap[String, Tree]()

      override final def inspect(tree: Tree): Unit = {
        tree match {
          case ClassDef(_, _, _, template) =>
            val methodsDeclaredAtThisLevel: Map[String, Tree] = template.children.collect {
              case d@DefDef(mods, TermName(name), _, _, _, _) 
                if mods.hasFlag(Flag.PRIVATE) && d.symbol.isMethod && !d.symbol.isAccessor =>
                  name -> d
            }.toMap
            methodsNotSeenYet ++= methodsDeclaredAtThisLevel
            continue(tree)
            methodsNotSeenYet.foreach { case (name, methodTree) =>
              context.warn(methodTree.pos, self, s"Dead code - private method $name not used anywhere: " + tree.toString().take(200))
            }
            methodsNotSeenYet --= methodsDeclaredAtThisLevel.keys

          case Select(_, TermName(name)) =>
            methodsNotSeenYet -= name
            continue(tree)

          case _ =>
            continue(tree)
        }
      }
    }
  }
}
