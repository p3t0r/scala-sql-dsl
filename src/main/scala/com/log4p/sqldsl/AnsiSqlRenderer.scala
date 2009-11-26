package com.log4p.sqldsl

case class SQL(val sql:String)

object AnsiSqlRenderer {
  implicit def query2sql(q:Query):SQL = SQL(sql(q))
  implicit def from2sql(f: From): SQL = SQL(sql(Query(f.operation.get, f, None)))

  def sql(query: Query): String = {
    List(
      expandOperation(query),
      expandFrom(query),
      expandWhere(query),
      expandOrder(query)
    )
    .filter(_ != None)
    .map(_ match {
      case Some(s) => s
      case s:String => s 
    })
    .mkString(" ")
  }

  def expandOperation(query:Query):String = query.operation match {
    case Select(field:String) => "select %s".format(field)
    case s:Select => "select %s".format(s.fields.mkString(","))    
    case _ => throw new IllegalArgumentException("Operation %s not implemented".format(query.operation))
  }

  def expandFrom(query: Query) = "from %s".format(query.from.table)
  def expandWhere(query: Query):Option[String] = {
    if (query.where.isEmpty || query.where.get.clauses.isEmpty)
      None
    else
      Option("where %s".format(query.where.get.clauses.map(expandClause(_)).mkString(" ")))
  }

  def expandClause(clause: Clause): String = clause match {
    case StringEquals(field, value) => "%s = %s".format(field, quote(value))
    case BooleanEquals(field, value) => "%s = %s".format(field, value)
    case NumberEquals(field, value) => "%s = %s".format(field, value)
    case in:In => "%s in (%s)".format(in.field, in.values.map(quote(_)).mkString(","))
    case and:And => "(%s and %s)".format(expandClause(and.lClause), expandClause(and.rClause))
    case or:Or => "(%s or %s)".format(expandClause(or.lClause), expandClause(or.rClause))
    case _ => throw new IllegalArgumentException("Clause %s not implemented".format(clause))
  }

  def expandOrder(query: Query):Option[String] = query.order match {
    case Some(direction) => direction match {
      case Asc(field) => Option("order by %s asc".format(field))
      case Desc(field) => Option("order by %s desc".format(field))
    }
    case None => None
  }

  def quote(value: String) = "'%s'".format(escape(value))
  def escape(value: String) = value.replaceAll("'", "''")
}