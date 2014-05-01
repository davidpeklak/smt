package smt

import org.scalatest.FunSuite
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import smt.migration.ScriptParsers

class ScriptParsersTest extends FunSuite with PropTesting {
  test("splitString") {

    case class Data(sep: String, script: String, expected: Seq[String])

    def partGen(sep: String) = alphaStr.filter(!_.contains(sep))

    def partSeqGen(sep: String) =
      for(n <- posNum[Int];
          parts <- listOfN(n, partGen(sep))) yield parts.toSeq

    val dataGen =
      for (sep <- alphaStr;
           parts <- partSeqGen(sep)) yield {
        val script = parts match {
          case Seq(onePart) => onePart
          case _ => parts.flatMap(part => Seq(sep, part)).tail.mkString("")
        }

        Data(sep, script, parts)
      }

    val p = forAll(dataGen)(data => {
      import data._
      ScriptParsers.splitString(sep)(script) == expected
    })

    check(p)
  }
}
