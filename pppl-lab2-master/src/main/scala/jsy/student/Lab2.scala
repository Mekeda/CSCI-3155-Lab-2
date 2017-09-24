package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Spencer Milbrandt
   * 
   * Partner: None
   * Collaborators: Jared Wright (Recitation TA), In-class Examples, Piazza Forums
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */


  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n // If it is a number, return n.
      case B(true) => 1.0 // If true, return 1.0, else false, return 0.0.
      case B(false) => 0.0
      // Try converting the string to a double, if unable to convert, throw NaN.
      case S(s) => try s.toDouble catch {case _: Throwable => Double.NaN}
      case Undefined => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b // If it's already a boolean, return it.
      case N(0.0) => false // 0.0 is false, any other number is true.
      case N(n) => { if (n.isNaN) false else true } // Except NaN false, else true.
      case S(s) => if (s == "") false else true// Empty strings are false (0.0) all other strings are true (non zero).
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => prettyNumber(n) // This checks if it is NaN, whole number, etc.
      case B(b) => { // Returns boolean value as a string.
        if (b) "true"
        else "false"
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      // Return themselves
      case N(n) => N(n)
      case S(s) => S(s)
      case B(b) => B(b)
      // Returns new environment
      case Var(x) => lookup(env, x)
      case Undefined => Undefined
      // If the conditional evaluates to true, evaluate first expression, otherwise evaluate the second expression.
      case If(e1, e2, e3) => if (toBoolean(eToVal(e1))) eToVal(e2) else eToVal(e3)

      /* Inductive Cases */

      case Unary(uop, e1) => uop match {
        case Not => B(!toBoolean(eToVal(e1)))
        case Neg => N(-toNumber(eToVal(e1)))
      }

      case ConstDecl(str, e1, e2) =>
        val v1 = eToVal(e1) // Evaluate e1 and set it equal to the var(str).
        eval(extend(env, str, v1), e2)

      //Match the Binary Operations
      case Binary(bop, e1, e2) => bop match  {

        // Right associative, recursive down the right.
        case Seq => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          v2 // Seq only returns the second expression.
        }

        // Arithmetic operations
        case Plus => {
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1,v2) match { // Match values of expressions.
            // When adding strings, in actuality it is concatenating.
            case (S(s1), e2) => S(toStr(S(s1))+toStr(eToVal(e2)))
            case (e1, S(s2)) => S(toStr(eToVal(e1))+toStr(S(s2)))
            // Otherwise add the numbers.
            case (_,_) => N(toNumber(v1) + toNumber(v2))
          }
        }
        case Minus => N(toNumber(eToVal(e1))-toNumber(eToVal(e2)))
        case Times => N(toNumber(eToVal(e1))*toNumber(eToVal(e2)))
        case Div => N(toNumber(eToVal(e1))/toNumber(eToVal(e2)))

        // Comparison Operators
        case Gt =>
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          // Evaluate each to match the values.
          (v1, v2) match{
            // In case of strings.
            case(S(s1),S(s2)) => B((s1)>(s2)) // Looking at length of string.
            case (S(s1), _) => {
              if (s1 == "") B(0>toNumber(v2)) // If it is an empty string set it to 0 and check.
              else B(toNumber(eToVal(v1))>toNumber(eToVal(v2)))
            }
            // If the second is a string.
            case (_, S(s2)) => {
              // If it is an empty string evaluate that the other number is larger than 0.
              if (s2 == "") B(toNumber(v1)>0)
              else B(toNumber(eToVal(v1))>toNumber(eToVal(v2)))
            }
            case(_,_) => B(toNumber(eToVal(v1))>toNumber(eToVal(v2)))
          }

        case Ge =>
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1,v2) match{
            case(S(s1),S(s2)) => B((s1)>=(s2))
            case (S(s1), _) => {
              if (s1 == "") B(0 >= toNumber(v2))
              else B(toNumber(eToVal(v1))>=toNumber(eToVal(v2)))
            }
            case (_, S(s2)) =>{
              if (s2 == "") B(toNumber(v1)>= 0)
              else B(toNumber(eToVal(v1))>=toNumber(eToVal(v2)))
            }
            case(_,_) => B(toNumber(eToVal(v1))>=toNumber(eToVal(v2)))
          }

        // Less than.
        case Lt =>
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1,v2) match{
            case(S(s1),S(s2)) => B((s1)<(s2))
            case (S(s1), _) => {
              if (s1 == "") B(0 < toNumber(v2))
              else B(toNumber(eToVal(v1))<toNumber(eToVal(v2)))
            }
            case (_, S(s2)) =>{
              if (s2 == "") B(toNumber(v1)<0)
              else B(toNumber(eToVal(v1))<toNumber(eToVal(v2)))
            }
            case(_,_) => B(toNumber(eToVal(v1))<toNumber(eToVal(v2)))
          }

        // Less than or Equal to.
        case Le =>
          val v1 = eToVal(e1)
          val v2 = eToVal(e2)
          (v1,v2) match{
            case(S(s1),S(s2)) => B((s1)<=(s2))
            case (S(s1), _) => {
              //evaluate empty strings to 0, then check
              if (s1 == "") B(0 <= toNumber(v2))
              else B(toNumber(eToVal(v1))<=toNumber(eToVal(v2)))
            }
            case (_, S(s2)) =>{
              if (s2 == "") B(toNumber(v1)<=0)
              else B(toNumber(eToVal(v1))<=toNumber(eToVal(v2)))
            }
            case(_,_) => B(toNumber(eToVal(v1))<=toNumber(eToVal(v2)))
          }

        /* Not equal or Equal
        These need to compare the boolean values of setting them either not equal to
        each other or equal to each other. As usual the expressions are continued to
        be evaluated before they are compared. */
        case Ne => B(eToVal(e1)!=eToVal(e2))
        case Eq => B(eToVal(e1)==eToVal(e2))

        /* AND and OR operations
        If the first is true evaluate the second. Otherwise return false (first)
        NOTE: This is an example of short circuiting and must be written this way
        so that e1 is only evaluated once. */
        case And => {
          val v1 = eToVal(e1)
          if (toBoolean(v1)) eToVal(e2)
          else v1
        }

        //If the first is true, return true, otherwise evaluate the second.
        case Or => {
          val v1 = eToVal(e1)
          if (toBoolean(v1)) v1
          else eToVal(e2)
        }

        case _ => ???
      }

      case Print (e1) => println (pretty (eToVal (e1) ) ); Undefined
      case _ => ???

    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }

}
