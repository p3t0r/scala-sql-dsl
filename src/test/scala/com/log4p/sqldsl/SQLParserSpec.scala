package com.log4p.sqldsl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class SQLParserSpec extends Spec with ShouldMatchers {
  val p = new SQLParser
  describe("given a sql string with an order clause") {
    describe("(when direction is asc)") {
      val sql = "select name from users order by name asc"
      it("should be parsed into an Asc object containing the given field") {
        val query = p.parse(sql).get

        query.operation should be (Select("name"))
        query.from should be (From("users"))
        query.order should be (Option(Asc("name")))
      }
    }
  }
  describe("given a sql string with a single where clause") {
    describe("(when equals predicate with string literal)") {
      val sql = "select name from users where name = \"peter\""
      it("should be parsed into an StringEquals object containing the given field and value") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select("name"))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (StringEquals("name","peter"))
      }
    }
    describe("(when equals predicate with numeric literal)") {
      val sql = """select age from users where age = 30"""
      it("should be parsed into an NumberEquals object containing the given field and value") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select("age"))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (NumberEquals("age",30))
      }
    }
  }
  describe("given a sql string with a combined where clause") {
    describe("(when equals predicate contains and)") {
      val sql = """select name from users where name = "peter" and age = 30"""
      it("should be parsed into an And object containing to correct subclauses") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select("name"))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (And(StringEquals("name","peter"), NumberEquals("age",30)))
      }
    }
    describe("(when equals predicate contains or)") {
      val sql = """select name from users where age = 20 or age = 30"""
      it("should be parsed into an Or object containing to correct subclauses") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select("name"))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (Or(NumberEquals("age",20), NumberEquals("age",30)))
      }
    }
    describe("(when equals predicate contains multiple combined clauses)") {
      val sql = """select name from users where name = "peter" and age = 20 or age = 30"""
      it("should be parsed into an Or object containing and And object and and Equals predicate") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select("name"))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (Or(And(StringEquals("name","peter"), NumberEquals("age",20)), NumberEquals("age",30)))
        println(AnsiSqlRenderer.sql(query))
      }
    }
    describe("(when equals predicate contains multiple combined clauses where the presedence is dictated by parens)") {
      val sql = """select name,age from users where name = "peter" and (active = true or age = 30)"""
      it("should be parsed into an Or object containing and And object and and Equals predicate") {
        println(sql)
        val query = p.parse(sql).get
        query.operation should be (Select(List("name","age"):_*))
        query.from should be (From("users"))
        query.where.get.clauses.head should be (And(StringEquals("name","peter"), Or(BooleanEquals("active",true), NumberEquals("age",30))))
        println(AnsiSqlRenderer.sql(query))
      }
    }

  }
}