/**
 * FunLang syntax analysis tests.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import FunLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing terminals

    test ("parsing an identifier of one letter produces the correct tree") {
        idnuse ("x") should parseTo[FunLangNode] (IdnUse("x"))
    }

    test ("parsing an identifier as an identifier produces the correct tree") {
        idnuse ("count") should parseTo[FunLangNode] (IdnUse("count"))
    }

    test ("parsing an identifier containing digits and underscores produces the correct tree") {
        idnuse ("x1_2_3") should parseTo[FunLangNode] (IdnUse("x1_2_3"))
    }

    test ("parsing an integer as an identifier gives an error") {
        idnuse ("42") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (digit)") {
        idnuse ("4foo") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (underscore)") {
        idnuse ("_f3") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a keyword as an identifier gives an error") {
        idnuse ("def") should failParseAt[FunLangNode] (1, 1,
                                                      "failure of not")
    }

    test ("parsing a keyword prefix as an identifier produces the correct tree") {
        idnuse ("defat") should parseTo[FunLangNode] (IdnUse("defat"))
    }

    test ("parsing an integer of one digit as an integer produces the correct tree") {
        factor ("8") should parseTo[FunLangNode] (IntExp(8))
    }

    test ("parsing an integer as an integer produces the correct tree") {
        integer ("99") should parseTo[String] ("99")
    }

    test ("parsing a non-integer as an integer gives an error") {
        integer ("total") should failParseAt[String] (1, 1,
            "string matching regex '[0-9]+' expected but 't' found")
    }

    // Tests of parsing basic expressions

    test ("parsing an equal expression produces the correct tree") {
        exp ("a == 1") should parseTo[FunLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a less than expression produces the correct tree") {
        exp ("a < 1") should parseTo[FunLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an addition expression produces the correct tree") {
        exp ("a + 1") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        exp ("a - 1") should parseTo[FunLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a multiplication expression produces the correct tree") {
        exp ("a * 1") should parseTo[FunLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a division expression produces the correct tree") {
        exp ("a / 1") should parseTo[FunLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an integer expression produces the correct tree") {
        exp ("823") should parseTo[FunLangNode] (IntExp (823))
    }

    test ("parsing a true expression produces the correct tree") {
        exp ("true") should parseTo[FunLangNode] (BoolExp (true))
    }

    test ("parsing a false expression produces the correct tree") {
        exp ("false") should parseTo[FunLangNode] (BoolExp (false))
    }

    test ("parsing an identifier expression produces the correct tree") {
        exp ("v123") should parseTo[FunLangNode] (IdnUse ("v123"))
    }

    test ("parsing a parenthesized expression produces the correct tree") {
        exp ("(a + 5)") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("parsing an application expression produces the correct tree") {
        exp ("a (b)") should parseTo[FunLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("parsing an if expression produces the correct tree") {
        exp ("if (true) then 3 else 4") should parseTo[FunLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    //                              //
    // Additional tests below here  //
    //                              //

    // Test for parsing block expressions
    test ("parsing a block produces the correct tree") {
        exp ("{ val a = 1  a + 1 + 2 }") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("a"),IntExp(1))),
                PlusExp(PlusExp(IdnUse("a"),IntExp(1)),IntExp(2))
            )
        )
    }

        test ("parsing a block with simple expression gives error") {
        exp ("{ 1 + 2 }") should failParseAt[FunLangNode] (
            1, 3, "'val' expected but '1' found"
        )
    }

    test ("parsing a empty block gives an error") {
        exp ("{ }") should failParseAt[FunLangNode] (
            1, 3, "'val' expected but '}' found"
        )
    }

    test ("parsing a unclosed right bracket block gives an error") {
        exp ("{ ") should failParseAt[FunLangNode] (
            1, 3, "'val' expected but end of source found"
        )
    }

    test ("parsing a unclosed left bracket block gives an error") {
        exp (" }") should failParseAt[FunLangNode] (1, 2, "exp expected")
    }

    test ("parsing a nested block produces the correct tree") {
        exp ("{ val a = 3 a + 1 } * { val a = 3 a + 1 }") should parseTo[FunLangNode] (
            StarExp(
                BlockExp(Vector(Val(IdnDef("a"),IntExp(3))),PlusExp(IdnUse("a"),IntExp(1))), 
                BlockExp(Vector(Val(IdnDef("a"),IntExp(3))),PlusExp(IdnUse("a"),IntExp(1)))
            )
        )
    }

    // Test for parsing application expressions
    test ("parsing a application expression produces the correct tree") {
        exp ("func (x)") should parseTo[FunLangNode] (
            AppExp (IdnUse ("func"), IdnUse ("x"))
        )
    }

    test ("parsing a application expression in a block with function produces the correct tree") {
        exp ("{ def func (x : Bool) = x func (abc)}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("x"),
                BoolType()),IdnUse("x"))))),AppExp(IdnUse("func"),IdnUse("abc"))
            )
        )
    }

        test ("parsing a application expression in a block with val produces the correct tree") {
        exp ("{ val a = b func (abc)}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("a"),
                IdnUse("b"))),AppExp(IdnUse("func"),IdnUse("abc"))
            )
        )
    }

    // Test for parsing conditions
    test ("parsing a complex if expression with identifiers produces the correct tree") {
        exp ("{ val a = 1 val b = 1 if (a == b) then a - 3 else a / 4 * 2 + 1 }") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("a"),IntExp(1)), Val(IdnDef("b"),IntExp(1))),
                IfExp(EqualExp(IdnUse("a"),IdnUse("b")),MinusExp(IdnUse("a"),IntExp(3)),PlusExp(StarExp(SlashExp(IdnUse("a"),IntExp(4)),IntExp(2)),IntExp(1)))
            )
        )
    }

    // Test for parsing function definitions
    test ("parsing a function definition produces the correct tree") {
        exp ("{ val x = 100 def func (a : Int) = a + 1 func (x) } ") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("x"),IntExp(100)), 
                FunGroup(
                    Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),IntType()),
                    PlusExp(IdnUse("a"),IntExp(1))))
                )), 
                AppExp(IdnUse("func"),IdnUse("x"))
            )
        )
    }

    test ("parsing a function definition with precendece produces the correct tree") {
        exp ("{ def func (a : Int) = a * 1 a + 1 }") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),IntType()),StarExp(IdnUse("a"),IntExp(1)))))),
                PlusExp(IdnUse("a"),IntExp(1))
            )
        )
    }

    test ("parsing a incorrect function definition produces the correct tree") {
        exp ("{ def def def }") should failParseAt[FunLangNode] (
            1, 7, "failure of not"
        )
    }

    // Test for parsing arguments
    test ("parsing arguments in a function produces the correct tree") {
        exp ("{ def func (a : Bool) = False * 1 1 }") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),BoolType()),StarExp(IdnUse("False"),IntExp(1)))))),
                IntExp(1))
        )
    }

    test ("parsing arguments outside a function gives an error") {
        exp ("{ (a : Int) }") should failParseAt[FunLangNode] (
            1, 3, "'val' expected but '(' found"
        )
    }

    test ("parsing incorrect arguments syntax gives an error") {
        exp ("{ def func (var a : Int) = a * 1 a + 1 }") should failParseAt[FunLangNode] (
            1, 17, "':' expected but 'a' found"
        )
    }

    // Test for parsing value definitions
    test ("parsing a value definition produces the correct tree") {
        exp ("{val a = 1 b + 1}") should parseTo[FunLangNode] (
            BlockExp (
                Vector(Val(IdnDef("a"), IntExp(1))),
                PlusExp(IdnUse("b"), IntExp(1))
            )
        )
    }

    test ("parsing a value definition with enclosed brackets gives an error ") {
        exp ("val a = 1 b + 1") should failParseAt[FunLangNode] (
            1, 1, "exp expected"
        )
    }

    // Test for parsing tipes
    test ("parsing a tipe with false or true produces the correct tree") {
        exp ("{def func (a : Bool) = False 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),BoolType()),IdnUse("False"))))),
                PlusExp(IntExp(1),IntExp(1))
            )
        )
    }

    test ("parsing a tipe associativity consecutively produces the correct tree") {
        exp ("{def func (a : Bool => Int => Bool => Int) = False 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),FunType(BoolType(),FunType(IntType(),FunType(BoolType(),IntType())))),IdnUse("False"))))),
                PlusExp(IntExp(1),IntExp(1))
            )
        )
    }

    test ("parsing a tipe associativity produces the correct tree") {
        exp ("{def func (a : Bool => Int) = False 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(FunGroup(Vector(Fun(IdnDef("func"),Arg(IdnDef("a"),FunType(BoolType(),IntType())),IdnUse("False"))))),
                PlusExp(IntExp(1),IntExp(1))
            )
        )
    }

    test ("parsing a tipe outside a function produces an incorrect tree") {
        exp ("a : Bool = False 1 + 1") should parseTo[FunLangNode] (
            IdnUse("a")
        )
    }
    
    test ("parsing a value with false or true produces the correct tree") {
        exp ("{val bool = false 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("bool"),BoolExp(false))),
                PlusExp(IntExp(1),IntExp(1))
            )
        )

        exp ("{val bool = true 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("bool"),BoolExp(true))),
                PlusExp(IntExp(1),IntExp(1))
            )
        )
    }

    test ("parsing a value with a number produces the correct tree") {
        exp ("{val int = 50 1 + 1}") should parseTo[FunLangNode] (
            BlockExp(
                Vector(Val(IdnDef("int"),IntExp(50))),
                PlusExp(IntExp(1),IntExp(1)))
        )
    }

    // Test for precedence
    test ("parsing precendence order of + and * produces the correct tree") {
        exp ("1 + 2 * 3") should parseTo[FunLangNode] (
            PlusExp(IntExp(1),StarExp(IntExp(2),IntExp(3)))
        )
    }

    test ("parsing if statement precedence order expressions produces the correct tree") {
        exp ("if (1 == 1) then 1 + 1 else 1 + 1 + 1") should parseTo[FunLangNode] (
            IfExp(
                EqualExp(IntExp(1),IntExp(1)),
                PlusExp(IntExp(1),IntExp(1)),
                PlusExp(PlusExp(IntExp(1),IntExp(1)),IntExp(1))
            )
        )
    }

    test ("parsing precendence order of * and + produces the correct tree") {
        exp ("1 * 2 + 3") should parseTo[FunLangNode] (
            PlusExp(StarExp(IntExp(1),IntExp(2)),IntExp(3))
        )
    }

    test ("parsing precendence order of - and / produces the correct tree") {
        exp ("1 - 2 / 3") should parseTo[FunLangNode] (
            MinusExp(IntExp(1),SlashExp(IntExp(2),IntExp(3)))
        )
    }

    test ("parsing precendence order expressions produces the correct tree") {
        exp ("1 / 2 * 3 + 4 - 5") should parseTo[FunLangNode] (
            MinusExp(PlusExp(StarExp(SlashExp(IntExp(1),IntExp(2)),IntExp(3)),IntExp(4)),IntExp(5))
        )
    }

    // Test for associativity
    test ("parsing left associativity order + expressions produces the correct tree") {
        exp ("1 + 2 + 3 + 4") should parseTo[FunLangNode] (
            PlusExp(PlusExp(PlusExp(IntExp(1),IntExp(2)),IntExp(3)),IntExp(4))
        )
    }

    test ("parsing left associativity order * expressions produces the correct tree") {
        exp ("1 * 2 * 3 * 4") should parseTo[FunLangNode] (
            StarExp(StarExp(StarExp(IntExp(1),IntExp(2)),IntExp(3)),IntExp(4))
        )
    }

    test ("parsing if statement associativity order expressions produces the correct tree") {
        exp ("if (1 == 2) then 2 + 3 else 1 + 2 + 3") should parseTo[FunLangNode] (
            IfExp(
                EqualExp(IntExp(1),IntExp(2)),
                PlusExp(IntExp(2),IntExp(3)),
                PlusExp(PlusExp(IntExp(1),IntExp(2)),IntExp(3))
            )
        )
    }

    test ("parsing if statement associativity order multiple expressions produces the correct tree") {
        exp ("if (1 == 2 + 3 - 4 + 5) then 1 - 2 + 3 else 1 + 2 ") should parseTo[FunLangNode] (
            IfExp(
                EqualExp(IntExp(1),PlusExp(MinusExp(PlusExp(IntExp(2),IntExp(3)),IntExp(4)),IntExp(5))),
                PlusExp(MinusExp(IntExp(1),IntExp(2)),IntExp(3)),
                PlusExp(IntExp(1),IntExp(2))
            )
        )
    }
}
