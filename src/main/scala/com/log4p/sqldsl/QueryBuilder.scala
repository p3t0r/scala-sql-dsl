package com.log4p.sqldsl

case class Query(val operation:Operation, val from: From, val where: Where, val order: Option[Direction] = None) {
  def order(dir: Direction) = this.copy(order = Option(dir))
}

abstract class Operation {
  def from(table: String) = From(this, table)
}
case class Select(val fields:String*) extends Operation

case class From(val operation:Operation, val table: String) {
  def where(clauses: Clause*): Query = Query(operation, this, Where(clauses: _*))
}

case class Where(val clauses: Clause*)

abstract class Clause {
  def and(otherField: Clause): Clause = And(this, otherField)
  def or(otherField: Clause): Clause = Or(this, otherField)
}

case class StringEquals(val f: String, val value: String) extends Clause
case class NumberEquals(val f: String, val value: Number) extends Clause
case class BooleanEquals(val f: String, val value: Boolean) extends Clause
case class In(val field: String, val values: String*) extends Clause
case class And(val clauses: Clause*) extends Clause
case class Or(val clauses: Clause*) extends Clause

abstract class Direction
case class Asc(field: String) extends Direction
case class Desc(field: String) extends Direction

object QueryBuilder {
  implicit def tuple2field(t: (String, String)): StringEquals = StringEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Int)): NumberEquals = NumberEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Boolean)): BooleanEquals = BooleanEquals(t._1, t._2)

  /** entrypoint for starting a select query */
  def select(fields:String*) = Select(fields:_*)
  def in(field: String, values: String*) = In(field, values: _*)
}