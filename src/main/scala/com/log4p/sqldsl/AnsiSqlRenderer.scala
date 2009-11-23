package com.log4p.sqldsl

case class SQL(val sql:String)

object AnsiSqlRenderer {
  implicit def query2sql(q:Query):SQL = SQL(sql(q))

  def sql(q: Query): String = {
    List(
      expandOperation(q),
      expandFrom(q),
      expandWhere(q),
      expandOrder(q)
    ).mkString(" ").trim
  }

  def expandOperation(q:Query):String = q.operation match {
    case Select(fields) => "select %s".format(fields.mkString(","))
    case _ => "<< unknown operation >>"
  }

  def expandFrom(q: Query) = "from %s".format(q.from.table)
  def expandWhere(q: Query) = "where %s".format(q.where.clauses.map(expandClause(_)).mkString(" "))

  def expandClause(clause: Clause): String = clause match {
    case StringEquals(field, value) => "%s = %s".format(field, quote(value))
    case BooleanEquals(field, value) => "%s = %s".format(field, value)
    case NumberEquals(field, value) => "%s = %s".format(field, value)
    case in:In => "%s in (%s)".format(in.field, in.values.map(quote(_)).mkString(","))
    case and:And => and.clauses.map(expandClause(_)).mkString("(", " and ", ")")
    case or:Or => or.clauses.map(expandClause(_)).mkString("(", " or ", ")")
    case _ => "<< unknown clause >>"
  }

  def expandOrder(q: Query) = q.order match {
    case Some(direction) => direction match {
      case Asc(field) => "order by %s asc".format(field)
      case Desc(field) => "order by %s desc".format(field)
    }
    case None => ""
  }

  def quote(value: String) = "'%s'".format(escape(value))
  def escape(value: String) = value.replaceAll("'", "''")
}