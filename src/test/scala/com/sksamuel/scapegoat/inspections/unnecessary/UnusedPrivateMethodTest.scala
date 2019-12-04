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

    "should not report warning" - {
      "for a private method which is called in another method" in {
        val code =
          """class Test {
            | private def foo(a:String, b:Int, c:Int) {
            |   s"$a $b $c"
            | }
            | def bar(): Unit = {
            |   println(foo("a", 1, 2))
            | }
            |}""".stripMargin

        compileCodeSnippet(code)
        compiler.scapegoat.feedback.warnings.size shouldBe 0
      }
      "for a private field (val) which is not used" in {
        val code =
          """class Test {
            | private val x = 3
            | def bar(): Unit = {
            |   println("Four")
            | }
            |}""".stripMargin

        compileCodeSnippet(code)
        compiler.scapegoat.feedback.warnings.size shouldBe 0
      }
    }

  }
}

