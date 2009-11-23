package com.log4p.sqldsl

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

import QueryBuilder._
import AnsiSqlRenderer._

class QueryBuilderSpec extends Spec with ShouldMatchers {
  
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
  }
}