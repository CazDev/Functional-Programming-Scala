/**
 * FunLang language execution tests.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import FunLangTree.ExpTree

/**
 * Tests that check that the translation works correctly.
 */
class ExecTests extends SemanticTests {

    import org.bitbucket.inkytonik.kiama.util.StringEmitter

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = parseProgram (str)
        val analysis = new SemanticAnalysis (new ExpTree (tree))
        import analysis._
        val messages = analysis.errors (tree)
        // println (messages)
        assert (messages.length === 0)

        val instrs = Translator.translate (tree)
        // println (instrs)

        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)

        machine.run (instrs) match {
            case _ : machine.State =>
                // Terminated correctly in some state
                assertResult (expected + "\n", "wrong execution output") (emitter.result ())
            case machine.FatalError (message) =>
                fail (message)
        }
    }

    test ("an integer expression evaluates to the correct result") {
        execTest ("""
            |1
            """.stripMargin,
            "1")
    }

    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4
            """.stripMargin,
            "7")
    }

    // FIXME: more tests here...

    // Boolean value tests
    test ("a true boolean expression evaluates to the correct result") {
        execTest (
             "true", 
             "true"
        )
    }

    test ("a false boolean expression evaluates to the correct result") {
        execTest (
            "false", 
            "false"
        )
    }

    // Identifier tests
    test ("an identifier expression evaluates to the correct result") {
        execTest (
            "{ val a = 5 val b = 1  a }", 
            "5"
        )
    }

    // Add expression tests
    test ("multiple add expression evaluates to the correct result") {
        execTest (
            "0 + 1 + 2 + 3", 
            "6"
        )
    }

    // Minus expression tests
    test ("a minus expression evaluates to the correct result") {
        execTest (
            "10 - 8", 
            "2"
        )
    }

    test ("multiple minus expression evaluates to the correct result") {
        execTest (
            "10 - 2 - 2", 
            "6"
        )
    }

    // Divide expression tests
    test ("a divide expression evaluates to the correct result") {
        execTest (
            "50 / 2", 
            "25"
        )
    }

    test ("a divide expression 5 / 5 evaluates to the correct result") {
        execTest (
            "5 / 5", 
            "1"
        )
    }

    test ("a divide expression 5 / 10 evaluates to the correct result") {
        execTest (
            "5 / 10", 
            "0"
        )
    }

    test ("multiple divide expressions evaluates to the correct result") {
        execTest (
            "20 / 2 / 2", 
            "5"
        )
    }

    // Multiplication expression tests
    test ("a Multiplication expression 20 * 5 evaluates to the correct result") {
        execTest (
            "20 * 5", 
            "100"
        )
    }

    test ("a multiply expression 5 * 20 evaluates to the correct result") {
        execTest (
            "5 * 20", 
            "100"
        )
    }

    test ("a multiply expression 6 * 7 evaluates to the correct result") {
        execTest (
            "6 * 7 * 2",  
            "84"
        )
    }

    // Equal expression tests
    test ("a false equal expression evaluates to the correct result") {
        execTest (
            "1 == 2", 
            "false"
        )
    }

    test ("a true equal expression evaluates to the correct result") {
        execTest (
            "3 == 3", 
            "true"
        )
    }

    // Less expression tests
    test ("a equal less expression evaluates to the correct result") {
        execTest (
            "3 < 3", 
            "false"
        )
    }

    test ("a false less expression evaluates to the correct result") {
        execTest (
            "4 < 3", 
            "false"
        )
    }

    test ("a true less expression evaluates to the correct result") {
        execTest (
            "2 < 3", 
            "true"
        )
    }

    // If conditional expression tests
    test ("a true if expression evaluates to the correct result") {
        execTest (
            "if (true) then 1 else 2", 
            "1"
        )
    }

    test ("a false if expression evaluates to the correct result") {
        execTest (
            "if (false) then 1 else 2", 
            "2"
        )
    }

    test ("a complex if expression evaluates to the correct result") {
        execTest (
            "if (5 == 4) then 1 * 2 else 2 + 3", 
            "5"
        )
    }

    // Block expression tests
    test ("a simple block expression evaluates to the correct result") {
        execTest (
            "{ val a = 1 a + 1 }", 
            "2"
        )
    }

    test ("a block expression evaluates to the correct result") {
        execTest (
            "{ val a = 5 val b = a + 1 a * b }", 
            "30"
        )
    }

    test ("a complex block expression evaluates to the correct result") {
        execTest (
            "{ val a = 5 val b = a * 2 if (a == b) then a - 2 else b + 2 }", 
            "12"
        )
    }

    // Function tests
    test ("a block with function evaluates to the correct result") {
        execTest (
            "{ val x = 15 def double (a : Int) = a * 2 double (x) }",
            "30"
        )
    }

    // Simple.fun
    test ("the example Simple.fun evaluates to the correct result") {
        execTest (
            "2 + 3 * 4",
            "14"
        )
    }

    // Block.fun
    test ("the example Block.fun evaluates to the correct result") {
        execTest (
            "{ val a = 5 val b = a + 1 a * b }",
            "30"
        )
    }

    // Function.fun
    test ("the example Function.fun evaluates to the correct result") {
        execTest (
            "{ val x = 100 def inc (a : Int) = a + 1 inc (x) }",
            "101"
        )
    }

    // Nesting.fun
    test ("the example Nesting.fun evaluates to the correct result") {
        execTest (
            "{ val a = 3 a + 1 } * { val b = 10 b - 1 }",
            "36"
        )
    }

}