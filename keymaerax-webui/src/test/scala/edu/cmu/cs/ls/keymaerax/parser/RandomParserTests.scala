/**
* Copyright (c) Carnegie Mellon University.
* See LICENSE.txt for the conditions of this license.
*/
package edu.cmu.cs.ls.keymaerax.parser

import edu.cmu.cs.ls.keymaerax.btactics.RandomFormula
import testHelper.KeYmaeraXTestTags.{CheckinTest, SlowTest, SummaryTest, UsualTest}
import testHelper.CustomAssertions.withSafeClue

import scala.collection.immutable._
import edu.cmu.cs.ls.keymaerax.core._
import edu.cmu.cs.ls.keymaerax.parser._
import edu.cmu.cs.ls.keymaerax.tools.KeYmaeraXTool
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

/**
 * Tests the parser on pretty prints of randomly generated formulas
  *
  * @author Andre Platzer
 */
class RandomParserTests extends FlatSpec with Matchers {
  val randomTrials = 4000
  val randomComplexity = 8
  val rand = new RandomFormula()


  val pp = KeYmaeraXPrettyPrinter
  //else new edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXWeightedPrettyPrinter
  val parser = if (false) KeYmaeraXParser else DLParser
  KeYmaeraXTool.init(Map.empty)

  def parseShouldBe(input: String, expr: Expression) = {
    val parse = parser.formulaParser(input)
    if (!(parse == expr)) {
      println("Reparsing" +
        "\nInput:      " + input +
        "\nParsed:     " + parse + " @ " + parse.getClass.getSimpleName +
        "\nExpression: " + KeYmaeraXPrettyPrinter.fullPrinter(parse))
      parse shouldBe expr
    }
  }

  "The parser" should "reparse pretty-prints of random formulas (checkin)" taggedAs(CheckinTest) in {test(10, 6)}
  it should "reparse pretty-prints of random formulas (summary)" taggedAs(SummaryTest) in {test(50, 6)}
  it should "reparse pretty-prints of random formulas (usual)" taggedAs(UsualTest) in {test(200,10)}
  it should "reparse pretty-prints of random formulas (slow)" taggedAs(SlowTest) in {test(randomTrials,20)}

  private def test(randomTrials: Int= randomTrials, randomComplexity: Int = randomComplexity) =
    for (i <- 1 to randomTrials) {
      val randClue = "Formula produced in\n\t " + i + "th run of " + randomTrials +
        " random trials,\n\t generated with " + randomComplexity + " random complexity\n\t from seed " + rand.seed

      val e = withSafeClue("Error generating random formula\n\n" + randClue) { rand.nextFormula(randomComplexity) }
      val output = withSafeClue("Error printing\n\n" + randClue) { pp.stringify(e) }

      withSafeClue("Random formula " + output + "\n\n" + randClue) {
        reparse(e)
      }
    }

  private def reparse(e: Expression) = {
    val printed = pp.stringify(e)
    println("Expression: " + printed)
    val full = pp.fullPrinter(e)
    println("Fullform:   " + full)
    parseShouldBe(full, e)
    println("Reparsing:  " + printed)
    parseShouldBe(printed, e)
    println("Fullparse:  " + pp.fullPrinter(parser(printed)))
  }

}
