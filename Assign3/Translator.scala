/**
 * FunLang to SEC translator.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

/**
 * Translator from FunLang source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import FunLangTree._
    import scala.collection.mutable.ListBuffer
    import SymbolTable._

    /**
     * Return a frame that represents the SEC instructions for a FunLang program.
     */
    def translate (program : Program) : Frame = {

        // An instruction buffer for accumulating the program instructions
        val programInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Translate the program by translating its expression.
         */
        val expInstrs = translateExpression (program.exp)
        programInstrBuffer.appendAll (expInstrs)
        programInstrBuffer.append (IPrint ())

        // Gather the program's instructions and return them
        programInstrBuffer.result ()

    }

    /**
     * Translate an expression and return a list of the instructions that
     * form the translation.
     */
    def translateExpression (exp : Exp) : Frame = {

        // An instruction buffer for accumulating the expression instructions
        val expInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : Instr) {
            expInstrBuffer.append (instr)
        }

        /**
         * Generate a sequence of instructions by appending them all to the
         * instruction buffer.
         */
        def genall (frame : Frame) {
            expInstrBuffer.appendAll (frame)
        }

        /**
         * Generate code to make a closure (argName => body).
         */
        def genMkClosure (argName : String, body : Exp) {
            val bodyInstrs = translateExpression (body)
            gen (IClosure (argName, bodyInstrs :+ IPopEnv ()))
        }

        /**
         * Translate FunLang expressions
         */
        exp match {
            
            // Value Instructions
            // Integer value
            case IntExp (value) =>
                gen (IInt (value))

            // Boolean value
            case BoolExp (value) =>
                gen (IBool (value))

            // Identifier Instruction
            case IdnUse (idn) =>
                gen (IVar (idn))

            // Arithmetic Instructions
            // Addition
            case PlusExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IAdd ())

            // Subtration
            case MinusExp(l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (ISub ())

            // Division
            case SlashExp(l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IDiv ())

            // Multiplication
            case StarExp(l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IMul ())

            // Equality comparison
            case EqualExp(l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IEqual ())

            // Less than comparison
            case LessExp(l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (ILess())

            // If conditional expression
            case IfExp(cond, thenExp, elseExp) => 
                genall(translateExpression(cond))
                gen(IBranch (translateExpression(thenExp), translateExpression(elseExp)))
            
            // Application expression
            case AppExp(fn, arg) =>
                genall(translateExpression(fn))
                genall(translateExpression(arg))
                gen(ICall())
            
            // Block expresion
            case BlockExp(defns, exp) =>
                defns match {
                    // Translate Val definition inside block expression
                    case Val(IdnDef(i), v) +: a =>
                        genMkClosure(i, BlockExp(a, exp))
                        genall(translateExpression(v))
                        gen(ICall())
                    // Translate function within block expression
                    case FunGroup (Fun(IdnDef(idn1), Lam(Arg(IdnDef(idn2),tipe), body)) +: funs) +: defns =>
                        genMkClosure( idn1, BlockExp(Vector(FunGroup(funs)) ++ defns, exp))
                        genMkClosure (idn2, body)
                        gen (ICall())
                    // Generate block expression for function definition
                    case FunGroup(_) +: defns => genall(translateExpression(BlockExp(defns,exp)))
                    // Generate all other inside block expressions
                    case _ =>
                        genall(translateExpression(exp))
                }
        }

        // Gather the expression's instructions and return them
        expInstrBuffer.result ()

    }

}
