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

//Alternativa con el Free Monad pattern

import cats._
import cats.free._
import freasymonad.cats.free


case class Juego(fraseSecreta: FraseSecreta, tableroUsuario: TableroUsuario, muerto: Muerto)

case class EstadoJuego(continua: Boolean, ganaste: Boolean)

case class FraseSecreta(frase: String)

case class TableroUsuario(tablero: String)

case class Muerto(vidas: Int)

case class Letra(letra: Char)


@free trait Ahorcado {

  type AhorcadoF[A] = Free[ADT, A]

  def generarMuerto(): AhorcadoF[Muerto]

  def mostrarMuerto(m: Muerto): AhorcadoF[Unit]

  def generarFraseSecreta(): AhorcadoF[FraseSecreta]

  def generarTableroUsuario(fraseSecreta: FraseSecreta): AhorcadoF[TableroUsuario]

  def mostrarTableroUsuario(tableroUsuario: TableroUsuario): AhorcadoF[Unit]

  def empezarJuego(): AhorcadoF[Juego] =
    for {
      muerto <- generarMuerto()
      frase <- generarFraseSecreta()
      tableroUsuario <- generarTableroUsuario(frase)
      _ <- mostrarTableroUsuario(tableroUsuario)
      _ <- mostrarMuerto(muerto)
    } yield Juego(frase, tableroUsuario, muerto)

  def pedirLetraAUsuario(): AhorcadoF[Letra]

  def hayCoincidencias(l: Letra, f: FraseSecreta): AhorcadoF[Boolean]

  def mostrarCoincidencias(l: Letra, f: FraseSecreta, t: TableroUsuario): AhorcadoF[TableroUsuario]

  def matarAlMuerto(leAtino: Boolean, m: Muerto): AhorcadoF[Muerto]

  def ronda(fraseSecreta: FraseSecreta,
            tableroUsuario: TableroUsuario,
            muerto: Muerto): AhorcadoF[Juego] = {
    for {
      letra <- pedirLetraAUsuario()
      coincidencia <- hayCoincidencias(letra, fraseSecreta)
      tablero <- mostrarCoincidencias(letra, fraseSecreta, tableroUsuario)
      masMuerto <- matarAlMuerto(coincidencia, muerto)
    } yield Juego(fraseSecreta, tablero, masMuerto)
  }

  def evaluarRonda(t: TableroUsuario, m: Muerto, f: FraseSecreta): AhorcadoF[EstadoJuego]

  def mostrarFin(b: Boolean): AhorcadoF[Unit]

  sealed trait ADT[A]

}

object AhorcadoProgram {

  import Ahorcado.ops._

  val program: AhorcadoF[Unit] =
    for {
      juego <- empezarJuego()
      ganaste <- jugar(juego.fraseSecreta, juego.tableroUsuario, juego.muerto, continua = true, ganaste = false)
      _ <- mostrarFin(ganaste)
    } yield ()

  def jugar(fraseSecreta: FraseSecreta,
            tableroUsuario: TableroUsuario,
            muerto: Muerto,
            continua: Boolean,
            ganaste: Boolean): AhorcadoF[Boolean] = {
    if (!continua) {
      for {
        estado <- evaluarRonda(tableroUsuario, muerto, fraseSecreta)
      } yield estado.ganaste
    }
    else {
      for {
        juego <- ronda(fraseSecreta, tableroUsuario, muerto)
        estado <- evaluarRonda(juego.tableroUsuario, juego.muerto, juego.fraseSecreta)
        ganaste <- jugar(juego.fraseSecreta, juego.tableroUsuario, juego.muerto, estado.continua, estado.ganaste)
      } yield ganaste
    }
  }


}

object ConsoleInterpreter {


  val consoleInterpreter = new Ahorcado.Interp[Id] {

    override def generarMuerto(): Id[Muerto] = Muerto(6)

    override def mostrarMuerto(m: Muerto): Id[Unit] = println("\nVidas = " + m.vidas)

    override def generarFraseSecreta(): Id[FraseSecreta] = FraseSecreta("EJEMPLO DE PRUEBA".toUpperCase)

    override def generarTableroUsuario(fraseSecreta: FraseSecreta): Id[TableroUsuario] =
      TableroUsuario(fraseSecreta.frase.map(c => {
        if (c.isWhitespace) ' '
        else '_'
      }))

    override def mostrarTableroUsuario(tableroUsuario: TableroUsuario): Id[Unit] =
      println(s"Bienvenido al mejor juego de Ahorcado! \n ${tableroUsuario.tablero}")

    override def pedirLetraAUsuario(): Id[Letra] = {
      println("\nAdivina una letra!")
      Letra(StdIn.readLine().toUpperCase.head)
    }

    override def hayCoincidencias(l: Letra, f: FraseSecreta): Id[Boolean] = {
      val coincidencias = f.frase.exists(c => c == l.letra)
      println(s"\nHay coincidencias? : $coincidencias")
      coincidencias
    }

    override def mostrarCoincidencias(l: Letra, f: FraseSecreta, t: TableroUsuario): Id[TableroUsuario] = {
      val posiciones = f.frase.map { c =>
        if (c == l.letra) true else false
      }
      val nuevoTablero = t.tablero.zip(posiciones).map { case (c, b) =>
        if (b) l.letra else c
      }
      val tablero = nuevoTablero.foldLeft("")((acc, c) => acc + c.toString)
      println(s"\n Asi esta la cosa: \n$tablero")
      TableroUsuario(tablero)
    }

    override def matarAlMuerto(leAtino: Boolean, m: Muerto): Id[Muerto] = {
      if (!leAtino) {
        println(s"\nTienes : ${m.vidas - 1} vidas!")
        Muerto(m.vidas - 1)
      } else {
        println(s"\nTienes : ${m.vidas} vidas!")
        m
      }
    }

    override def evaluarRonda(t: TableroUsuario, m: Muerto, f: FraseSecreta): EstadoJuego = {
      val aciertosJugador = t.tablero.count(c => c != '_')
      val aciertosJuego = f.frase.filterNot(c => c.isWhitespace).length
      val restante = f.frase.length - aciertosJuego
      if (aciertosJugador - restante == aciertosJuego) EstadoJuego(continua = false, ganaste = true)
      else if (m.vidas == 0) EstadoJuego(continua = false, ganaste = false)
      else EstadoJuego(continua = true, ganaste = false)
    }

    override def mostrarFin(b: Boolean): Id[Unit] =
      if (b) println("Felicidades! Ganaste!")
      else println("Hijoles, perdiste de nuevo!")

  }

}

object Application extends App {

  ConsoleInterpreter.consoleInterpreter.run(AhorcadoProgram.program)

}