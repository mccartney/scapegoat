package com.sksamuel.scapegoat.inspections.unnecessary

import com.sksamuel.scapegoat.PluginRunner
import com.sksamuel.scapegoat.inspections.unneccesary.UnusedPrivateMethod
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

class UnusedPrivateMethodTest extends FreeSpec
  with Matchers with PluginRunner with OneInstancePerTest {

  override val inspections = Seq(new UnusedPrivateMethod)

  "UnusedPrivateMethod" - {
    "should report warning" - {
      "for a single private method in a class" in {
        val code =
          """class Test {
             | private def foo(a:String, b:Int, c:Int) {
             |   s"$a $b $c"
             | }
             |}""".stripMargin

        compileCodeSnippet(code)
        compiler.scapegoat.feedback.warnings.size shouldBe 1
      }
    }
  }
}

