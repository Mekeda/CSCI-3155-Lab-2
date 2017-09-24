package jsy.student

import jsy.lab2.Lab2Like
import jsy.lab2.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

/*
 * This file contains a number of *Spec classes that define a set of
 * tests.
 *
 * All of the tests are gathered together in Lab2Suite.
 */

class Lab2Spec(lab2: Lab2Like) extends FlatSpec {
  import lab2._

  "And" should "return true only if both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(And,t,t)) === t)
    assert(eval(Binary(And,t,f)) === f)
    assert(eval(Binary(And,f,t)) === f)
    assert(eval(Binary(And,f,f)) === f)
  } 
 
  it should "return non-intuitive results from differing types" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))
  }
 
  "Or" should "return true if either or both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(Or,t,t)) === t)
    assert(eval(Binary(Or,f,t)) === t)
    assert(eval(Binary(Or,t,f)) === t)
    assert(eval(Binary(Or,f,f)) === f)
  }

  it should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }
  
  "Plus" should "add two number values and return a number" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  "Minus" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  "Times" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  "Div" should "divide two number values and return a number" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1.6))
  }

  "Arithmetic Operators" should "produce non-intuitive solutions given differing expression types" in {
    val e1 = B(true)
    val e2 = N(7)
    assert(eval(Binary(Plus,e1,e2)) == N(8))
  }

  "Eq" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Ne" should "return true if two numerical values are different" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(false))
  }

  "Lt" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  } 

  "Le" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 

  "Gt" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is not strictly greater than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 

  "Ge" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  } 
  
  it should "return false if the first expression is less than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(false))
  } 
  
  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Comparisons" should "produce non-intuitive results given the expressions given" in {
    val e1 = N(5)
    val e2 = Undefined
    assert(eval(Binary(Eq,e1,e2)) === B(false))
  } 

  "ConstDecl" should "extend the environment with the first expression results bound to the identifier, and then eval the second expression" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = eval(ConstDecl("x", e1, e2)) 
    assert(e3 === N(4))
  } 
  
  "If" should "eval the first expression if the conditional is true" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(true), e1, e2)) 
    assert(e3 === N(5))
  } 
  
  it should "eval the second expression if the conditional is false" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(false), e1, e2)) 
    assert(e3 === N(2))
  } 
  
  "Seq" should "execute the first expression, followed by the second, and should eval to the second expression" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(Binary(Seq, e1, e2)) 
    assert(e3 === N(2))
  } 
  
  "Neg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(-5))
  } 
  
  "Not" should "return the compliment of a boolean value" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Unary(Not, e1))
    val e4 = eval(Unary(Not, e2))
    assert(e3 === B(false))
    assert(e4 === B(true))
  }

  "toNumber" should "return any integer value as a Double" in {
    val n1 = 3
    val n2 = 1.5
    val n3 = -12
    assert(toNumber(N(n1)) == 3.0)
    assert(toNumber(N(n2)) == 1.5)
    assert(toNumber(N(n3)) == -12.0)
  }

  "toNumber" should "return the corresponding 1.0 for true or 0.0 for false" in {
    val t = B(true)
    val f = B(false)

    assert(toNumber(t) == 1.0)
    assert(toNumber(f)==0.0)
  }

  "toNumber" should "return string as a double" in {
    val s1 = "1.0"
    val s2 = "0.0"
    val s3 = "-7"
    assert(toNumber(S(s1)) == 1.0)
    assert(toNumber(S(s2)) == 0.0)
    assert(toNumber(S(s3)) == -7.0)
  }

  "toStr" should "return 'undefined' for a string or for Undefined input" in {
    val s = Undefined
    assert(toStr(s)== "undefined")
  }


  "toStr" should "return number as string object for any number" in {
    val v1 = 4
    val v2 = 0
    val v3 = -7
    assert(toStr(N(v1))=="4")
    assert(toStr(N(v2)) =="0")
    assert(toStr(N(-7)) =="-7")
  }

  "toStr" should "return boolean values as strings" in {
    val b1 = true
    val b2 = false
    assert(toStr(B(b1))=="true")
    assert(toStr(B(b2))=="false")
  }

  "toBoolean" should "return the same boolean value for either true or false" in {
    val b1 = true
    val b2 = false
    assert(toBoolean(B(b1)) == true)
    assert(toBoolean(B(b2)) == false)
  }

  "toBoolean" should "return false for 0.0 ans true otherwise" in {
    val n1 = 1
    val n2 = 0.0
    val n3 = 15
    val n4 = -2
    assert(toBoolean(N(n1))==true)
    assert(toBoolean(N(n2))==false)
    assert(toBoolean(N(n3))==true)
    assert(toBoolean(N(n4))==true)
  }

  "toBoolean" should "return false for any Double.Nan, any empty string, or Undefined" in {
    val n1 = Double.NaN
    val s1 = ""
    val u1 = Undefined
    assert(toBoolean(N(n1)) == false)
    assert(toBoolean(S(s1)) == false)
    assert(toBoolean(u1) == false)
  }

  "And1" should "return non-intuitive results from differing types" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(0))

    val e4 = N(5)
    val e5 = B(true)
    val e6 = eval(Binary(And, e4, e5))
    assert(e6 === B(true))

    val e7 = B(true)
    val e8 = N(5)
    val e9 = eval(Binary(And, e7, e8))
    assert(e9 === N(5))

    val e10 = N(5)
    val e11 = B(false)
    val e12 = eval(Binary(And, e10, e11))
    assert(e12 === B(false))

    val e13 = B(false)
    val e14 = N(5)
    val e15 = eval(Binary(And, e13, e14))
    assert(e15 === B(false))
  }

  "And2" should "return non-intuitive results from string types" in {
    val e1 = S("hi")
    val e2 = N(1)
    val e3 = eval(Binary(And, e1, e2))
    assert(e3 === N(1))

    val e4 = S("hi")
    val e5 = B(true)
    val e6 = eval(Binary(And, e4, e5))
    assert(e6 === B(true))

    val e7 = N(1)
    val e8 = S("hi")
    val e9 = eval(Binary(And, e7, e8))
    assert(e9 === S("hi"))

    val e10 = S("hi")
    val e11 = B(false)
    val e12 = eval(Binary(And, e10, e11))
    assert(e12 === B(false))

    val e13 = N(0)
    val e14 = S("hi")
    val e15 = eval(Binary(And, e13, e14))
    assert(e15 === N(0))
  }

  "Or1" should "return true if either or both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(eval(Binary(Or,t,t)) === t)
    assert(eval(Binary(Or,f,t)) === t)
    assert(eval(Binary(Or,t,f)) === t)
    assert(eval(Binary(Or,f,f)) === f)
  }

  "Or2" should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = eval(Binary(Or, e1, e2))
    assert(e3 === N(5))

    val e4 = S("hi")
    val e5 = B(true)
    val e6 = eval(Binary(Or, e4, e5))
    assert(e6 == S("hi"))
  }

  "Or3" should "return non-intuitive results from boolean types" in {
    val e7 = B(true)
    val e8 = N(5)
    val e9 = eval(Binary(Or, e7, e8))
    assert(e9 === B(true))

    val e10 = N(5)
    val e11 = B(false)
    val e12 = eval(Binary(Or, e10, e11))
    assert(e12 === N(5))

    val e13 = B(false)
    val e14 = N(5)
    val e15 = eval(Binary(Or, e13, e14))
    assert(e15 === N(5))
  }

  "Plus1" should "add two number values and return a number" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  "Plus2" should "concatenate two strings or a number and a string" in {
    val e1 = S("he")
    val e2 = S("llo")
    val e3 = eval(Binary(Plus, e1, e2))
    assert(e3 === S("hello"))

    val e4 = S("he")
    val e5 = S("7")
    val e6 = eval(Binary(Plus, e4, e5))
    assert(e6 === S("he7"))
  }

  "Minus1" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  "Minus2" should "subtract should return the negative number when subtracted from zero" in {
    val e1 = N(0)
    val e2 = N(3)
    val e3 = eval(Binary(Minus, e1, e2))
    assert(e3 === N(-3))
  }

  "Times1" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = eval(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  "Div1" should "divide two number values and return a number" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(1.6))
  }

  "Div2" should "return zero when zero is divided by a number" in {
    val e1 = N(0)
    val e2 = N(5)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(0))
  }

  "Div3" should "return infinity when divided by zero" in {
    val e1 = N(8)
    val e2 = N(0)
    val e3 = eval(Binary(Div, e1, e2))
    assert(e3 === N(Double.PositiveInfinity))
  }

  "Arithmetic Operators1" should "produce non-intuitive solutions given differing expression types" in {
    val e1 = B(true)
    val e2 = N(7)
    assert(eval(Binary(Plus,e1,e2)) == N(8))

    val e3 = B(true)
    val e4 = B(false)
    assert(eval(Binary(Minus, e3, e4)) == N(1))
  }

  "Eq1" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  "Eq2" should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Eq3" should "return true if two strings values are the same" in {
    val e1 = S("csci")
    val e2 = S("csci")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  "Eq4" should "return false if two strings values are not the same" in {
    val e1 = S("csci")
    val e2 = S("hola")
    val e3 = eval(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Ne1" should "return true if two numerical values are different" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  }

  "Ne2" should "return false if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(false))
  }

  "Ne3" should "return true for true and false comparison" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  }

  "Lt1" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  "Lt2" should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Lt3" should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Lt4" should "return false if the first expression is greater than the second" in {
    val e1 = S("hola")
    val e2 = N(7)
    val e3 = eval(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Le1" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  "Le2" should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  "Le3" should "return false if the case of a number and string" in {
    val e1 = N(2)
    val e2 = S("hello")
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  "Le4" should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  "Gt1" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  }

  "Gt2" should "return true if the first boolean is greater than the second" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  }

  "Gt3" should "return false if the first expression is not strictly greater than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))

    val e4 = S("")
    val e5 = S("hi")
    val e6 = eval(Binary(Gt, e4, e5))
    assert(e6 === B(false))

  }

  "Gt4" should "return false one is a number and the other is a string" in {
    val e1 = N(9)
    val e2 = S("hello")
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))

    val e4 = S("hey")
    val e5 = N(1)
    val e6 = eval(Binary(Gt, e4, e5))
    assert(e6 === B(false))
  }

  "Gt5" should "return true for true greater than false or true greater than zero" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(true))

    val e4 = B(true)
    val e5 = N(0)
    val e6 = eval(Binary(Gt, e4, e5))
    assert(e6 === B(true))
  }

  "Gt6" should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  }

  "Ge1" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Ge2" should "return false if the first expression is less than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(false))
  }

  "Ge3" should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Ge4" should "return true if the number and boolean values are the same" in {
    val e1 = N(1)
    val e2 = B(true)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))

    val e4 = N(0)
    val e5 = B(false)
    val e6 = eval(Binary(Ge, e4, e5))
    assert(e6 === B(true))
  }

  "Ge5" should "return true for true greater than false" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Comparisons1" should "produce non-intuitive results given the expressions given" in {
    val e1 = N(5)
    val e2 = Undefined
    assert(eval(Binary(Eq,e1,e2)) === B(false))
  }

  "Comparisons2" should "should evaluate boolean compared to corresponding numbers or strings to false" in {
    val e1 = B(true)
    val n1 = N(1)
    val s1 = S("true")
    val e2 = B(false)
    val n2 = N(0)
    val s2 = S("false")
    assert(eval(Binary(Eq,e1,n1)) === B(false))
    assert(eval(Binary(Eq,e2,n2)) === B(false))
    assert(eval(Binary(Eq,e1,s1)) === B(false))
    assert(eval(Binary(Eq,e2,s2)) === B(false))
  }

  "ConstDecl1" should "extend the environment with the first expression results bound to the identifier, and then eval the second expression" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = eval(ConstDecl("x", e1, e2))
    assert(e3 === N(4))

    val e4 = N(7)
    val e5 = Binary(Times, Var("x"), N(-1))
    val e6 = eval(ConstDecl("x", e4, e5))
    assert(e6 === N(-7))
  }

  "If1" should "eval the first expression if the conditional is true" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(true), e1, e2))
    assert(e3 === N(5))
  }

  "If2" should "eval the second expression if the conditional is false" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(If(B(false), e1, e2))
    assert(e3 === N(2))
  }

  "If3" should "eval the first expression if the conditional eval to true" in {
    val e1 = Binary(Minus, N(3), N(1))
    val e2 = Binary(Times, N(2), N(4))
    val e3 = eval(Binary(Gt, N(3), N(0)))
    val e4 = eval(If(e3, e1, e2))
    assert(e4 === N(2))
  }

  "If4" should "eval the second expression if the conditional eval to false" in {
    val e1 = Binary(Minus, N(3), N(1))
    val e2 = Binary(Times, N(2), N(4))
    val e3 = eval(Binary(Lt, N(3), N(0)))
    val e4 = eval(If(e3, e1, e2))
    assert(e4 === N(8))
  }

  "Neg1" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = eval(Unary(Neg, e1))
    assert(e2 === N(-5))
  }

  "Neg2" should "return -1 for true" in {
    val b1 = true
    val e2 = eval(Unary(Neg, B(b1)))
    assert(e2 === N(-1))
  }

  "Not1" should "return the compliment of a boolean value" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = eval(Unary(Not, e1))
    val e4 = eval(Unary(Not, e2))
    assert(e3 === B(false))
    assert(e4 === B(true))
  }

  "Not2" should "retun true for zero and false for any other number" in {
    val n1 = 1.0
    val n2 = 0
    val n3 = -3
    val e1 = eval(Unary(Not, N(n1)))
    val e2 = eval(Unary(Not, N(n2)))
    val e3 = eval(Unary(Not, N(n3)))
    assert(e1 === B(false))
    assert(e2 === B(true))
    assert(e3 === B(false))
  }

  "Not3" should "return true for an empty string and false for any other string" in {
    val s1 = ""
    val s2 = " "
    val s3 = "hey"
    val e1 = eval(Unary(Not, S(s1)))
    val e2 = eval(Unary(Not, S(s2)))
    val e3 = eval(Unary(Not, S(s3)))
    assert(e1 === B(true))
    assert(e2 === B(false))
    assert(e3 === B(false))
  }

  "Seq1" should "execute the first expression, followed by the second, and should eval to the second expression" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = eval(Binary(Seq, e1, e2))
    assert(e3 === N(2))

    val e4 = Binary(Minus, N(4), N(2))
    val e5 = Binary(Times, N(4), N(2))
    val e6 = eval(Binary(Seq, e4, e5))
    assert(e6 === N(8))

    val e7 = Binary(Times, N(2), N(3))
    val e8 = Binary(Plus, S("a"), S("b"))
    val e9 = eval(Binary(Seq, e7, e8))
    assert(e9 === S("ab"))
  }
}

// An adapter class to pass in your Lab2 object.
class Lab2SpecRunner extends Lab2Spec(Lab2)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab2.
// The test expects a corresponding .ans file with the expected result.
class Lab2JsyTests extends JavascriptyTester(None, "lab2", Lab2)

class Lab2Suite extends Suites(

  new Lab2SpecRunner,
  new Lab2JsyTests
)

