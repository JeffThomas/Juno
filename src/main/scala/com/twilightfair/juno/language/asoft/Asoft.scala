package com.twilightfair.juno.language.asoft

import com.twilightfair.juno.Constants._
import com.twilightfair.juno.language.Language
import com.twilightfair.juno.language.asoft.elements._
import com.twilightfair.juno.language.asoft.parsers._
import com.twilightfair.juno.language.expressionator.elements._
import com.twilightfair.juno.lexx.{LexxConfig, Lexx}
import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.elements.{BlockElement, StringElement, NopElement}
import com.twilightfair.juno.parse.parsers._
import com.twilightfair.juno.parse.{Element, Out, Parser, TokenParser}
import com.twilightfair.juno.runtime.{Runner, Process, Value}

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

/**
 * Created by jthomas on 12/11/13.
 */

object Asoft extends Language with Runner {

  val lexxConfig = new LexxConfig {
   override def matchers(location:Int): List[MatcherState] =
      List(
        IdentifierMatcher.init(location, keywords, "_", "$%"),
        SymbolMatcher.init(location, symbols),
        IntegerMatcher.init(location),
        FloatMatcher.init(location),
        WhitespaceMatcher.init(location, false),
        StringMatcher.init(location)
      )
  }

  def parserData = HashMap[String, Any](
    "If" -> List[IfElement](),
    "For" -> HashMap[String, ForElement](),
    "On" -> false,
    "Rem" -> false,
    "Step" -> None,
    "Dim" -> false,
    "Labels" -> HashMap[String, Element](),
    "PrintCR" -> true,
    "OnGosub" -> None,
    "Data" -> List[String]()
  )

  val symbols: List[String] =
    List("+", "-", "/", "*", "(", ")", "++", ",", ";", ":", "<", ">", "<>", "< >", "<=", ">=", "!=", "==", "=", "\n", "\r", "\r\n")

  val keywords: List[String] =
    List("PRINT", "FOR", "TO", "NEXT", "IF", "THEN", "TAB", "GOTO", "GOSUB", "RETURN", "INPUT", "ON", "STEP", "REM", "DIM", "DATA", "READ", "RESTORE")

  override val prefixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Integer", None, LeafParser(IntegerElement, PRECEDENCE_NONE)),
      ("Float", None, LeafParser(FloatElement, PRECEDENCE_NONE)),
      ("String", None, LeafParser(StringElement, PRECEDENCE_NONE)),
      ("Ident", None, PrefixIdentifierParser(IdentifierElement, PRECEDENCE_NONE)),
      ("Symbol", Some("-"), PrefixParser(PrefixMathElement, PRECEDENCE_NONE)),
      ("Keyword", Some("PRINT"), PrefixParser(PrintElement, PRECEDENCE_NONE)),
      ("Keyword", Some("GOSUB"), PrefixParser(GosubElement, PRECEDENCE_NONE)),
      ("Keyword", Some("RETURN"), LeafParser(ReturnElement, PRECEDENCE_NONE)),
      ("Keyword", Some("IF"), PrefixParser(IfElement, PRECEDENCE_NONE)),
      ("Keyword", Some("THEN"), LeafParser(ThenElement, PRECEDENCE_NONE)),
      ("Keyword", Some("ON"), PrefixOnParser(OnElement, PRECEDENCE_NONE)),
      ("Keyword", Some("FOR"), PrefixParser(ForElement, PRECEDENCE_NONE)),
      ("Keyword", Some("STEP"), PrefixParser(StepElement, PRECEDENCE_NONE)),
      ("Keyword", Some("NEXT"), NextParser(NextElement, PRECEDENCE_NONE)),
      ("Keyword", Some("DIM"), PrefixParser(DimElement, PRECEDENCE_NONE)),
      ("Keyword", Some("REM"), PrefixParser(NextElement, PRECEDENCE_NONE)),
      ("Keyword", Some("DATA"), DataParser(EOLElement, PRECEDENCE_NONE)),
      ("Keyword", Some("READ"), PrefixParser(ReadElement, PRECEDENCE_NONE)),
      ("Keyword", Some("INPUT"), PrefixParser(InputElement, PRECEDENCE_NONE)),
      ("Keyword", Some("RESTORE"), PrefixParser(RestoreElement, PRECEDENCE_NONE)),
      ("Symbol", Some("("), BlockParser(BlockElement, PRECEDENCE_CALL)),
      ("Symbol", Some("\r\n"), LeafParser(EOLElement, PRECEDENCE_END)),
      ("Symbol", Some("\r"), LeafParser(EOLElement, PRECEDENCE_END)),
      ("Symbol", Some("\n"), LeafParser(EOLElement, PRECEDENCE_END)),
      ("Symbol", Some(":"), LeafParser(SeperatorElement, PRECEDENCE_END)),
      ("Symbol", Some(")"), BlockEndParser(BlockElementRPN, PRECEDENCE_PREFIX))
  )

  override val infixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Symbol", Some("+"), InfixParser(InfixMathElement, PRECEDENCE_SUM)),
      ("Symbol", Some("-"), InfixParser(InfixMathElement, PRECEDENCE_SUM)),
      ("Symbol", Some("*"), InfixParser(InfixMathElement, PRECEDENCE_PRODUCT)),
      ("Symbol", Some("/"), InfixParser(InfixMathElement, PRECEDENCE_PRODUCT)),
      ("Symbol", Some("++"), PostfixParser(PostfixMathElement, PRECEDENCE_POSTFIX)),
      ("Symbol", Some("="), InfixParser(InfixAssignmentElement, PRECEDENCE_ASSIGNMENT)),
      ("Symbol", Some("<"), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some(">"), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some("=="), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some("!="), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some("<>"), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some("< >"), InfixParser(InfixConditionalElement, PRECEDENCE_CONDITIONAL)),
      ("Symbol", Some(";"), InfixConcatParser(InfixConcatElement, PRECEDENCE_POSTFIX)),
      ("Symbol", Some(";"), NoCRParser(NopElement, PRECEDENCE_PREFIX)),
      ("Keyword", Some("GOTO"), InfixParser(OnGotoElement, PRECEDENCE_SUM)),
      ("Keyword", Some("GOSUB"), InfixParser(OnGosubElement, PRECEDENCE_SUM)),
      ("Keyword", Some("TO"), InfixParser(InfixToElement, PRECEDENCE_SUM)),
      ("Symbol", Some(","), InfixParser(CommaConcatElement, PRECEDENCE_PRODUCT))
    )

  override def buildProcess(out: Out): Process = {
    Process(this, Nil, Nil, HashMap[String, Any](
      "Labels" -> (HashMap[String, Element]() ++= out.parser.data("Labels").asInstanceOf[HashMap[String, Any]].seq.asInstanceOf[HashMap[String, Element]]),
      "Namespace" -> HashMap[String, Value](),
      "CallStack" -> ArrayBuffer[Element](),
      "ReadIndex" -> 0,
      "Data" -> out.parser.data("Data").asInstanceOf[List[String]].map(Value(_))
    ), Nil)
  }

  @tailrec override final def runExpression(proc: Process): Future[Process] = {
    if (proc.runStack.headOption.isEmpty)
      Future.successful(proc)
    else {
      runExpression(proc.runStack.headOption.get(proc.copy(runStack = proc.runStack.tail)))
    }
  }

  override def buildParser(lexer: Lexx): Parser = {
    Parser(lexer, this, None, None, parserData)
  }

  def graph(script: Stream[Char]): String = {
    val out: Out = parse(script)
    val graphOut = out.elements map { element =>
      element.graph("")._2
      //println(element.asInstanceOf[ElementRPN].rpn(""))
    }
    graphOut.mkString("")
  }

  def tree(script: Stream[Char]): String = {
    val out: Out = parse(script)
    val buffer = new StringBuilder()
    val treeOut = out.elements map { element =>
      element.tree("", buffer)
    }
    buffer.toString
  }

  override def preElement(token: Parser.ParseLex, data: HashMap[String, Any]): (TokenParser, HashMap[String, Any]) = {
    val tkn = token.lexx.currentToken.get
    token.lexx.currentToken.map {
      case int if int.tokenType == "Integer" && token.lexx.newLine =>
        ((LeafParser(LabelElement, PRECEDENCE_PREFIX)), data)
      case key if key.tokenType == "Keyword" && !data("On").asInstanceOf[Boolean] && key.value == "GOTO" =>
        ((PrefixParser(GotoElement, PRECEDENCE_NONE)), data)
      case key if key.tokenType == "Keyword" && !data("On").asInstanceOf[Boolean] && key.value == "GOSUB" =>
        ((PrefixParser(GosubElement, PRECEDENCE_NONE)), data)
      case _ => (token.parser, data)
    }.get
  }
}
