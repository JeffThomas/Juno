package test.com.twilightfair.juno.language.expressionator

import com.twilightfair.juno.language.expressionator.Expressionator

import org.specs2.mutable._

/**
 * Created by jthomas on 12/11/13.
 */

class ExpressionatorSpec extends Specification {

  def testForSingleValue(script: String, value: Float) = {
    val res = Expressionator.run(script.toStream)
    (res map { process =>
      process.heap.size mustEqual 1
      process.heap(0).value.asInstanceOf[Float] mustEqual value
    }).await
  }

  "The Expressionator Language" should {
    "add two numbers" in {
      testForSingleValue("2 + 4", 6)
    }
    "add a bunch of numbers" in {
      testForSingleValue("1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0", 45)
    }
    "subtract two numbers" in {
      testForSingleValue("2 - 4", -2)
    }
    "subtract a negative" in {
      testForSingleValue("2 - -8", 10)
    }
    "add two numbers subtract one" in {
      testForSingleValue("2 + 4 - 1", 5)
    }
    "multiply two numbers" in {
      testForSingleValue("2 * 4", 8)
    }
    "follow multiplier precedence" in {
      testForSingleValue("2 - 1 * 4", -2)
    }
    "follow scope precedence" in {
      testForSingleValue("(2 - 1) * 4", 4)
    }
    "follow postfix precedence" in {
      testForSingleValue("2++ * 4", 12)
    }
    "follow scope precedence 2" in {
      testForSingleValue("6 + (2 - 1) * 4", 10)
    }
    "follow scope precedence 3" in {
      testForSingleValue("6   / (4 -  1) + 1", 3)
    }
    "a more complex formula" in {
      testForSingleValue("2 / (7++ + (3 - 4) * 6) - -5", 6)
    }
    "parse error" in {
      val res = Expressionator.run("1 + d + 2".toStream)
      (res map { process =>
        process.errors.size mustEqual 1
      }).await
    }
  }
}
