package com.log4p.sqldsl

case class SQL(val sql:String)

object AnsiSqlRenderer {
  implicit def query2sql(q:Query):SQL = SQL(sql(q))
  implicit def from2sql(f: From): SQL = SQL(sql(Query(f.operation, f, Where())))

  def sql(query: Query): String = {
    List(
      expandOperation(query),
      expandFrom(query),
      expandWhere(query),
      expandOrder(query)
    ).mkString(" ").trim
  }

  def expandOperation(query:Query):String = query.operation match {
    case Select(fields) => "select %s".format(fields.mkString(","))
    case _ => throw new IllegalArgumentException("Operation %s not implemented".format(query.operation))
  }

  def expandFrom(query: Query) = "from %s".format(query.from.table)
  def expandWhere(query: Query) = {
    if (query.where.clauses.isEmpty)
      ""
    else
      "where %s".format(query.where.clauses.map(expandClause(_)).mkString(" "))
  }

  def expandClause(clause: Clause): String = clause match {
    case StringEquals(field, value) => "%s = %s".format(field, quote(value))
    case BooleanEquals(field, value) => "%s = %s".format(field, value)
    case NumberEquals(field, value) => "%s = %s".format(field, value)
    case in:In => "%s in (%s)".format(in.field, in.values.map(quote(_)).mkString(","))
    case and:And => and.clauses.map(expandClause(_)).mkString("(", " and ", ")")
    case or:Or => or.clauses.map(expandClause(_)).mkString("(", " or ", ")")
    case _ => throw new IllegalArgumentException("Clause %s not implemented".format(clause))
  }

  def expandOrder(query: Query) = query.order match {
    case Some(direction) => direction match {
      case Asc(field) => "order by %s asc".format(field)
      case Desc(field) => "order by %s desc".format(field)
    }
    case None => ""
  }

  def quote(value: String) = "'%s'".format(escape(value))
  def escape(value: String) = value.replaceAll("'", "''")
}