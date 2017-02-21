package visitor


trait Visitable {
  def accept(v: Visitor): Unit
}

sealed trait Articulo {
  val nombre: String
  val descripcion: String
  val precio: BigDecimal
}

case class CocaCola() extends Articulo with Visitable {
  override val nombre: String = "Coca-Cola"
  override val descripcion: String = "Deliciosa bebida de Coca-Cola co."
  override val precio: BigDecimal = BigDecimal(9.99)
  override def accept(v: Visitor): Unit = v.visit(this)
}

case class Guitarra() extends Articulo with Visitable {
  override val nombre: String = "Guitarra Fender"
  override val descripcion: String = "Guitarra Fender XS-9000"
  override val precio: BigDecimal = BigDecimal(25000.00)
  override def accept(v: Visitor): Unit = v.visit(this)
}

case class Lapiz() extends Articulo with Visitable {
  override val nombre: String = "Lapiz Mirado"
  override val descripcion: String = "Lapiz Mirado 2BH"
  override val precio: BigDecimal = BigDecimal(6.50)
  override def accept(v: Visitor): Unit = v.visit(this)
}

trait Visitor {
  def visit(c: CocaCola): Unit
  def visit(g: Guitarra): Unit
  def visit(lapiz: Lapiz): Unit
}

object ImpuestoVisitor extends Visitor {
  override def visit(c: CocaCola): Unit = println(s"El impuesto de la coca es de: ${c.precio * .16}")
  override def visit(g: Guitarra): Unit = println(s"El impuesto de la guitarra es de: ${g.precio * .15}")
  override def visit(l: Lapiz): Unit = println(s"El impuesto del lapiz es de: ${l.precio * .20}")
}

object Tienda extends App {
  //Usando el patrón Visitor
  val articulos = List(CocaCola(), Guitarra(), Lapiz())
  articulos foreach { _.accept(ImpuestoVisitor) }

  //Usando Pattern-Matching
  def calcularImpuesto(a: Articulo): Unit = a match {
    case c@CocaCola() =>  println(s"El impuesto de la coca es de: ${c.precio * .16}")
    case g@Guitarra() => println(s"El impuesto de la guitarra es de: ${g.precio * .15}")
    case l@Lapiz() => println(s"El impuesto del lapiz es de: ${l.precio * .20}")
  }
  articulos foreach { articulo => calcularImpuesto(articulo) }
}

