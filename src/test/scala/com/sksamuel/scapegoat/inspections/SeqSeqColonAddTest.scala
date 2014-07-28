package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat.PluginRunner
import org.scalatest.{OneInstancePerTest, FreeSpec, Matchers}

/** @author Stephen Samuel */
class SeqSeqColonAddTest extends FreeSpec with ASTSugar with Matchers with PluginRunner with OneInstancePerTest {

  override val inspections = Seq(new SeqSeqColonAdd)

  "lists using colon add with list" - {
    "should report warning" in {
      val code = """object Test {
                   |  val a = List(1, 2, 3)
                   |  val b = List(4, 5, 6)
                   |  val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 1
    }
  }

  "seqs using colon add with list" - {
    "should report warning" in {
      val code = """object Test {
                   |  val a = Seq(1, 2, 3)
                   |  val b = List(4, 5, 6)
                   |  val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 1
    }
  }

  "lists using colon add with seq" - {
    "should report warning" in {
      val code = """object Test {
                   |  val a = List(1, 2, 3)
                   |  val b = Seq(4, 5, 6)
                   |  val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 1
    }
  }

  "seqs using colon add with seq" - {
    "should report warning" in {
      val code = """object Test {
                   |  val a = Seq(1, 2, 3)
                   |  val b = Seq(4, 5, 6)
                   |  val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 1
    }
  }

  "seqs using colon add with non seq" - {
    "should not report warning" in {
      val code = """object Test {
                   |        val a = Seq(1, 2, 3)
                   |        val b = "string"
                   |        val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 0
    }
  }

  "lists using colon add with non seq" - {
    "should not report warning" in {
      val code = """object Test {
                   |        val a = List(1, 2, 3)
                   |        val b = "string"
                   |        val c = a :+ b
                    } """.stripMargin

      compileCodeSnippet(code)
      compiler.scapegoat.reporter.warnings.size shouldBe 0
    }
  }
}
