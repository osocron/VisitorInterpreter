package interpreter

import java.util

import scala.io.StdIn

trait Expression {
  def interpret(): Int
}

case class Add(leftExpression: Expression,
               rightExpression: Expression) extends Expression {

  override def interpret(): Int = leftExpression.interpret() + rightExpression.interpret()

}

case class Product(leftExpression: Expression,
                   rightExpression: Expression) extends Expression {

  override def interpret(): Int = leftExpression.interpret() * rightExpression.interpret()

}

case class Substract(leftExpression: Expression,
                     rightExpression: Expression) extends Expression {

  override def interpret(): Int = leftExpression.interpret() - rightExpression.interpret()

}

case class Number(n: Int) extends Expression {

  override def interpret(): Int = n

}

object ExpressionUtils {

  def isOperator(s: String): Boolean = {
    if (s == "+" || s == "*" || s == "-") true
    else false
  }

  def getOperator(s: String, left: Expression, right: Expression): Expression = s match {
    case "+" => Add(left, right)
    case "*" => Product(left, right)
    case "-" => Substract(left, right)
  }

}

object InterpreterTest extends App {
  val tokenString = "7 3 - 2 1 + *"
  val stack = new util.Stack[Expression]()
  val tokenArray = tokenString.split(" ")
  for (s <- tokenArray) {
    if (ExpressionUtils.isOperator(s)) {
      val rightExpression = stack.pop()
      val leftExpression = stack.pop()
      val operator = ExpressionUtils.getOperator(s, leftExpression, rightExpression)
      val result = operator.interpret()
      stack.push(Number(result))
    } else {
      val i = Number(Integer.parseInt(s.toString))
      stack.push(i)
    }
  }
  println(s"($tokenString): ${stack.pop().interpret()}")
}

//Alternativa con ADT's

sealed trait Option[A]{
  /**
    * Equivalente del método interpret()
    *
    * @param z  El contexto que se le pasa al intérprete
    * @param f  El significado que se le dará a cada expresión del lenguaje
    * @tparam B El tipo de dato que regresa el intérprete
    * @return
    */
  def fold[B](z: B)(f: A => B): B = this match {
    case None() => z
    case Some(a) => f(a)
  }
}
case class None[A]() extends Option[A]
case class Some[A](a: A) extends Option[A]

object FunInterpreterTest extends App {

  val algunNumero: Option[Int] = Some(5)
  val ningunNumero: Option[Int] = None()

  def double(opN: Option[Int]): Int = opN.fold(0)(_ * 2)

  println(s"Algun numero: ${double(algunNumero)}\nNingun numero: ${double(ningunNumero)}")

}

//Alternativa con el Free Monad pattern

import cats._
import cats.free._
import freasymonad.cats.free

@free trait Interaction {

  type InteractionF[A] = Free[GrammarADT, A]
  sealed trait GrammarADT[A]

  def ask(a: String): InteractionF[String]
  def tell(msg: String): InteractionF[Unit]

}

object InteractionProgram {

  import Interaction.ops._

  def program: InteractionF[Unit] =
    for {
      answer <- ask("How are you doing?")
      _ <- tell(s"It's goot that you are doing $answer")
    } yield ()

}

object ConsoleTest extends App {

  val consoleInterpreter = new Interaction.Interp[Id] {
    override def ask(a: String): Id[String] = {
      println(a)
      StdIn.readLine()
    }
    override def tell(msg: String): Id[Unit] = println(msg)
  }

  consoleInterpreter.run(InteractionProgram.program)

}

object SpeechTest extends App {
  import edu.cmu.sphinx.api._
  import scala.util.control.Breaks._

  val configuration = new Configuration
  configuration.setAcousticModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us")
  configuration.setDictionaryPath("resource:/edu/cmu/sphinx/models/en-us/cmudict-en-us.dict")
  configuration.setLanguageModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us.lm.bin")

  val speechInterpreter = new Interaction.Interp[Id] {
    override def ask(a: String): Id[String] = {
      println(a)
      val speechRecognizer = new LiveSpeechRecognizer(configuration)
      speechRecognizer.startRecognition(true)
      var result = speechRecognizer.getResult
      var answer: String = ""
      breakable { while(true) {
        val word = speechRecognizer.getResult.getHypothesis
        if (word.endsWith("change")) break()
        else {
          println(word)
          answer = answer + " "  + word
        }
      }}
      answer
    }
    override def tell(msg: String): Id[Unit] = println(msg)
  }

  speechInterpreter.run(InteractionProgram.program)

}
