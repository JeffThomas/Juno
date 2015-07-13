package com.twilightfair.juno.language.test

import com.twilightfair.juno.parse.elements.configs.{InfixConfig, ElementConfig}
import com.twilightfair.juno.parse.ElementFactory
import com.twilightfair.juno.language.expressionator.elements.{IntegerElement, InfixMathElement, FloatElement}

/**
 * Created by jthomas on 6/19/14.
 */
trait ElementRPN {
  def rpn(buf: String): String
}

class IntegerElementRPN(config: ElementConfig)
  extends IntegerElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    buf + value + " "
  }
}
object IntegerElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new IntegerElementRPN(config)))
}

class FloatElementRPN(config: ElementConfig)
  extends FloatElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    buf + value + " "
  }
}
object FloatElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new FloatElementRPN(config)))
}

class InfixMathElementRPN(config: InfixConfig)
  extends InfixMathElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    right.asInstanceOf[ElementRPN].rpn(left.asInstanceOf[ElementRPN].rpn(buf)) + operator + " "
  }
}
object InfixMathElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixMathElementRPN(config.asInstanceOf[InfixConfig])))
}
