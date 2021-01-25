/**
 * FunLang syntax analyser.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for FunLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import FunLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Exp] =
        phrase (program)

    lazy val program : PackratParser[Exp] =
        exp

    // Precendence level 0 (block and app)
    lazy val exp : PackratParser[Exp] =
        app |
        // left associative, therefore goto exp1
        exp1

    // Precendence level 1 (conditional expressions)
    lazy val exp1 : PackratParser[Exp] =
        cond |
        // left associative, therefore goto exp2
        exp2

    // Precendence level 2 (equal and less than)
    lazy val exp2 : PackratParser[Exp] =
        exp3 ~ ("==" ~> exp3) ^^ {case e1 ~ e2 => EqualExp(e1,e2)} |
        exp3 ~ ("<" ~> exp3) ^^ {case e1 ~ e2 => LessExp(e1,e2)} |
        // not associative, therefore goto exp3
        exp3

    // Precendence level 3 (addition and subtraction)
    lazy val exp3 : PackratParser[Exp] = 
        exp3 ~ ("+" ~> exp4) ^^ {case e1 ~ e2 => PlusExp(e1,e2)} |
        exp3 ~ ("-" ~> exp4) ^^ {case e1 ~ e2 => MinusExp(e1,e2)} |
        // left associative, therefore goto exp4
        exp4

    // Precendence level 4 (multiplication and division)
    lazy val exp4 : PackratParser[Exp] = 
        exp4 ~ ("*" ~> factor) ^^ {case e1 ~ e2 => StarExp(e1,e2)} |
        exp4 ~ ("/" ~> factor) ^^ {case e1 ~ e2 => SlashExp(e1,e2)} |
        // left associative, therefore goto factor
        factor

    // Precendence level 5 (all other kinds of expressions)
    lazy val factor : PackratParser[Exp] = 
        "false" ^^ (_ => BoolExp (false)) |
        "true" ^^ (_ => BoolExp (true)) |
        idnuse |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")" |
        exp |
        block |
        failure ("exp expected")

     lazy val definitions : PackratParser[Vector[DefnGroup]] =
         (defngroup+) ^^ Vector[DefnGroup]

     lazy val defngroup : PackratParser[DefnGroup] =
         (fundefn+) ^^ FunGroup | valdefn

    lazy val block : PackratParser[Exp] =
        //block : "{" definitions exp "}".
        ("{" ~> definitions) ~ (exp <~ "}") ^^ { 
            case def1 ~ exp1 => BlockExp(def1, exp1)
        }

    lazy val app : PackratParser[Exp] = 
        //idnuse "(" exp ")".
        identifier ~ ("(" ~> identifier <~ ")") ^^ { 
            case id1 ~ id2 => AppExp(IdnUse(id1), IdnUse(id2)) 
        }

    lazy val cond : PackratParser[Exp] = 
        //cond : "if" "(" exp ")" "then" exp "else" exp.
        ("if" ~> "(" ~> exp <~ ")") ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ { 
            case exp1 ~ exp2 ~ exp3 => IfExp(exp1,exp2,exp3) 
        }

    lazy val fundefn : PackratParser[Fun] =
        //"def" idndef "(" arg ")" "=" exp.
        ("def" ~> identifier) ~ ("(" ~> arg <~ ")") ~ ("=" ~> exp) ^^ {
            case id ~ ar ~ exp => Fun(IdnDef(id), ar, exp)
        }

    lazy val arg : PackratParser[Arg] =
        identifier ~ (":" ~> tipe) ^^ {
            case id ~ ti => Arg(IdnDef(id), ti)
        }

    lazy val valdefn : PackratParser[Val] =
        //"val" idndef "=" exp.
        ("val" ~> identifier) ~ ("=" ~> exp) ^^ { 
            case id ~ exp => Val(IdnDef(id), exp) 
        }

    lazy val tipe : PackratParser[Type] =
        //"some type" ^^^ Type()
        // tipe will recurse back to itself
        tipes ~ ("=>" ~> tipe) ^^ { case tipe1 ~ tipe2 => FunType(tipe1,tipe2) } |
        tipes
 
    lazy val tipes : PackratParser[Type] =
        //"some type" ^^^ Type()
        "Int" ^^^ IntType() |
        "Bool" ^^^ BoolType() |
        "(" ~> tipe <~ ")"

    // NOTE: You should not need to change anything below here... 

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        identifier ^^ IdnDef

    lazy val idnuse =
        identifier ^^ IdnUse

    val keywordStrings =
        List ("def", "else", "false", "if", "then", "true", "val")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    override val whitespace : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "/*" ~ rep (not ("*/") ~ (comment | any)) ~ "*/" |
        "//.*\n".r

}
