package com.log4p.sqldsl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import QueryBuilder._
import AnsiSqlRenderer._

class QueryBuilderSpec extends Spec with ShouldMatchers {

  val SQL = QueryBuilder

  describe("A Query") {
    describe("(when containing nested 'and' or 'or' clauses)") {
      val q = select ("*") from ("user") where (("name","peter") and (("active", true) or ("role", "admin")))
      
      it("should contain parentheses at the correct locations in the resulting SQL") {
        q.sql should be ("select * from user where (name = 'peter' and (active = true or role = 'admin'))")
      }
    }
    describe("(when containing quotes in values)") {
      val q = select ("*") from ("user") where (("name","p'eter"))

      it("should escape those quotes in the resulting SQL") {
        q.sql should be ("select * from user where name = 'p''eter'")
      }
    }
    describe("(when testing against numeric value)") {
      val q = select ("*") from("user") where (("id", 100))

      it("shouldn't quote numbers in resulting SQL") {
        q.sql should be ("select * from user where id = 100")
      }
    }
    describe("(when containing 'in' clause)") {
      val q = select ("*") from ("user") where (in("name","pe'ter","petrus"))

      it("should generate in-clause with escaped values in SQL") {
        q.sql should be ("select * from user where name in ('pe''ter','petrus')")
      }
    }
    describe("(when ordered)") {
      val q = select ("*") from ("user") where (("name","peter")) order Desc("name")

      it("should generate order SQL") {
        q.sql should be ("select * from user where name = 'peter' order by name desc")
      }
    }
    describe("(when asked to select 'all)") {
      val q = select ('all) from ("user") where (("id", 100))
      
      it("should select *") {
        q.sql should be ("select * from user where id = 100")
      }
    }
    describe("(when asked to select using another symbol)") {
      it("should throw an exception, for now") {
        evaluating { val q = select ('bla) from ("user") where (("id", 100)) } should produce[Throwable]
      }
    }
    describe("(when no where clause is specified)") {
      val q = select ('all) from ("user")

      it("should select all rows without filter") {
        q.sql should be ("select * from user")
      }
    }
    describe("(when a where clause is not, but an order clause is specified)") {
      val q = select ('all) from ("user") order Asc("name")

      it("should select and order all rows without filter") {
        q.sql should be ("select * from user  order by name asc")
      }
    }
    describe("(when specified with lots less parentheses)") {
      val q = SQL select 'all from "user" where (("id", 100))

      it("should still work") {
        q.sql should be ("select * from user where id = 100")
      }
    }
  }
}