package smt

import org.scalatest.FunSuite
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers
import smt.migration.FileSplitters
import GenUtil._

class ScriptParsersTest extends FunSuite with Checkers with PropTesting {
  test("splitString") {

    case class Data(sep: String, script: String, expected: Seq[String])

    def partGen(sep: String) = alphaStr.waitFor(!_.contains(sep))

    def partSeqGen(sep: String) =
      for(n <- posNum[Int];
          parts <- listOfN(n, partGen(sep))) yield parts.toSeq

    val dataGen =
      for (sep <- nonEmptyAlphaStr;
           parts <- partSeqGen(sep)) yield {
        val script = parts match {
          case Seq(onePart) => onePart
          case _ => parts.flatMap(part => Seq(sep, part)).tail.mkString("")
        }

        Data(sep, script, parts)
      }

    val p = forAll(dataGen)(data => {
      import data._
      FileSplitters.splitString(sep)(script) == expected
    })

    check(p)
  }
}
