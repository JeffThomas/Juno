package com.twilightfair.juno.language.expressionator.elements

import com.twilightfair.juno.parse.elements.configs._
import com.twilightfair.juno.parse.elements.BlockElement
import com.twilightfair.juno.parse.ElementFactory

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

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object IntegerElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new IntegerElementRPN(config)))
}

class FloatElementRPN(config: ElementConfig)
  extends FloatElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    buf + value + " "
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object FloatElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new FloatElementRPN(config)))
}

class InfixMathElementRPN(config: InfixConfig)
  extends InfixMathElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    right.asInstanceOf[ElementRPN].rpn(left.asInstanceOf[ElementRPN].rpn(buf)) + operator + " "
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object InfixMathElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixMathElementRPN(config.asInstanceOf[InfixConfig])))
}

class InfixSeparatorElementRPN(config: InfixConfig)
  extends InfixSeparatorElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    right.asInstanceOf[ElementRPN].rpn(left.asInstanceOf[ElementRPN].rpn(buf)) + operator + " "
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object InfixSeparatorElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixSeparatorElementRPN(config.asInstanceOf[InfixConfig])))
}

class PrefixMathElementRPN(config: PrefixConfig)
  extends PrefixMathElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    right.asInstanceOf[ElementRPN].rpn(buf) + operator + " "
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object PrefixMathElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new PrefixMathElementRPN(config.asInstanceOf[PrefixConfig])))
}

class PostfixMathElementRPN(config: PostfixConfig)
  extends PostfixMathElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    left.asInstanceOf[ElementRPN].rpn(buf) + operator + " "
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object PostfixMathElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new PostfixMathElementRPN(config.asInstanceOf[PostfixConfig])))
}

class BlockElementRPN(config: BlockConfig)
  extends BlockElement(config) with ElementRPN {

  override def rpn(buf: String): String = {
    block.foldLeft(buf)((buf2, e) => e.asInstanceOf[ElementRPN].rpn(buf2))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???
}
object BlockElementRPN extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new BlockElementRPN(config.asInstanceOf[BlockConfig])))
}
