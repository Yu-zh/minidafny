import scala.util.parsing.combinator._
import java.io.FileReader


object VCGen {

  /* Array type (not sure whether this works) */
  trait Arr

  case class Varr(name: String) extends Arr
  case class Warr(arr: Arr, ind: ArithExp, value: ArithExp) extends Arr
  /* Arithmetic expressions. */
  trait ArithExp

  case class Num(value: Int) extends ArithExp
  case class Var(name: String) extends ArithExp
//  case class Read(name: String, ind: ArithExp) extends ArithExp
  case class Add(left: ArithExp, right: ArithExp) extends ArithExp
  case class Sub(left: ArithExp, right: ArithExp) extends ArithExp
  case class Mul(left: ArithExp, right: ArithExp) extends ArithExp
  case class Div(left: ArithExp, right: ArithExp) extends ArithExp
  case class Mod(left: ArithExp, right: ArithExp) extends ArithExp
  case class Parens(a: ArithExp) extends ArithExp
  // because I want my read to be able to read from a modified array
  case class Read(arr: Arr, ind: ArithExp) extends ArithExp

  /* Comparisons of arithmetic expressions. */
  type Comparison = Product3[ArithExp, String, ArithExp]


  /* Boolean expressions. */
  trait BoolExp

  case class BCmp(cmp: Comparison) extends BoolExp
  case class BNot(b: BoolExp) extends BoolExp
  case class BDisj(left: BoolExp, right: BoolExp) extends BoolExp
  case class BConj(left: BoolExp, right: BoolExp) extends BoolExp
  case class BParens(b: BoolExp) extends BoolExp


  /* Statements and blocks. */
  trait Statement
  type Block = List[Statement]

  case class Assign(x: String, value: ArithExp) extends Statement
  case class Write(x: String, ind: ArithExp, value: ArithExp) extends Statement
  case class ParAssign(x1: String, x2: String, value1: ArithExp, value2: ArithExp) extends Statement
  case class If(cond: BoolExp, th: Block, el: Block) extends Statement
  case class While(cond: BoolExp, body: Block) extends Statement
  case class WhileInv(cond: BoolExp, body: Block, inv: List[Assertion]) extends Statement
  case class ParWrite(x1: String, ind1: ArithExp, x2: String, ind2: ArithExp, value1: ArithExp, value2: ArithExp) extends Statement

  trait Assertion
  case class ACmp(cmp: Comparison) extends Assertion
  case class ANot(a: Assertion) extends Assertion
  case class AAnd(left: Assertion, right: Assertion) extends Assertion
  case class AOr(left: Assertion, right: Assertion) extends Assertion
  case class AImply(pre: Assertion, post: Assertion) extends Assertion
  // case class AForall(x: List[String], a: Assertion) extends Assertion
  // case class AExist(x: List[String], a: Assertion) extends Assertion
  case class AForall(x: List[TypedId], a: Assertion) extends Assertion
  case class AExist(x: List[TypedId], a: Assertion) extends Assertion
  case class AParen(a: Assertion) extends Assertion
  case class ATrue() extends Assertion
  /* Complete programs. */
  type Program = Product2[String, Block]
  type HoareProgram = Product4[String, List[Assertion], List[Assertion], Block]

  trait TypedId
  case class ArrId(id: String) extends TypedId
  case class IntId(id: String) extends TypedId
  case class DummyId(id: String) extends TypedId

  object ImpParser extends RegexParsers {
    /* General helpers. */
    def pvar  : Parser[String] = "\\p{Alpha}(\\p{Alnum}|_)*".r

    /* Parsing for ArithExp. */
    def num   : Parser[ArithExp] = "-?\\d+".r ^^ (s => Num(s.toInt))
    def atom  : Parser[ArithExp] =
      "(" ~> aexp <~ ")" |
      pvar ~ ("[" ~> aexp <~ "]") ^^ {case v ~ i => Read(Varr(v), i)} |
      num | pvar ^^ { Var(_) } |
      "-" ~> atom ^^ { Sub(Num(0), _) }
    def factor: Parser[ArithExp] =
      atom ~ rep("*" ~ atom | "/" ~ atom | "%" ~ atom) ^^ {
        case left ~ list => (left /: list) {
          case (left, "*" ~ right) => Mul(left, right)
          case (left, "/" ~ right) => Div(left, right)
          case (left, "%" ~ right) => Mod(left, right)
        }
      }
    def term  : Parser[ArithExp] =
      factor ~ rep("+" ~ factor | "-" ~ factor) ^^ {
        case left ~ list => (left /: list) {
          case (left, "+" ~ right) => Add(left, right)
          case (left, "-" ~ right) => Sub(left, right)
        }
      }
    def aexp  : Parser[ArithExp] = term

    /* Parsing for Comparison. */
    def comp  : Parser[Comparison] =
      aexp ~ ("=" | "<=" | ">=" | "<" | ">" | "!=") ~ aexp ^^ {
        case left ~ op ~ right => (left, op, right)
      }

    /* Parsing for BoolExp. */
    def batom : Parser[BoolExp] =
      "(" ~> bexp <~ ")" | comp ^^ { BCmp(_) } | "!" ~> batom ^^ { BNot(_) } // last batom should be bexp
    def bconj : Parser[BoolExp] =
      batom ~ rep("&&" ~> batom) ^^ {
        case left ~ list => (left /: list) { BConj(_, _) }
      }
    def bdisj : Parser[BoolExp] =
      bconj ~ rep("||" ~> bconj) ^^ {
        case left ~ list => (left /: list) { BDisj(_, _) }
      }
    def bexp  : Parser[BoolExp] = bdisj

    /* Parsing for Statement and Block. */
    def block : Parser[Block] = rep(stmt)
    def stmt  : Parser[Statement] =
      pvar ~ ("[" ~> aexp <~ "]") ~ (":=" ~> aexp <~ ";") ^^ {
        case v ~ i ~ e => Write(v, i, e)
      } |
      (pvar <~ ":=") ~ (aexp <~ ";") ^^ {
        case v ~ e => Assign(v, e)
      } |
      (pvar <~ ",") ~ (pvar <~ ":=") ~ (aexp <~ ",") ~ (aexp <~ ";") ^^ {
        case v1 ~ v2 ~ e1 ~ e2 => ParAssign(v1, v2, e1, e2)
      } |
      pvar ~ ("[" ~> aexp <~ "]") ~ ("," ~> pvar) ~ ("[" ~> aexp <~ "]") ~ (":=" ~> aexp) ~ ("," ~> aexp <~ ";") ^^ {
        case a1 ~ i1 ~ a2 ~ i2 ~ v1 ~ v2 => ParWrite(a1, i1, a2, i2, v1, v2)
      } |
      ("if" ~> bexp <~ "then") ~ (block <~ "else") ~ (block <~ "end") ^^ {
        case c ~ t ~ e => If(c, t, e)
      } |
      ("if" ~> bexp <~ "then") ~ (block <~ "end") ^^ {
        case c ~ t => If(c, t, Nil)
      } |
      ("while" ~> (bexp /* ~ rep("inv" ~ assn) */) <~ "do") ~ (block <~ "end") ^^ {
        case c ~ b => While(c, b)
      } |
      ("while" ~> (bexp ~ rep("inv" ~> assn)) <~ "do") ~ (block <~ "end") ^^ {
        case c ~ assns ~ b => WhileInv(c, b, assns)
      }

    /* Parsing for Assertion. */

    def aatom : Parser[Assertion] =
      "(" ~> assn <~ ")" ^^ { AParen(_) } | comp ^^ { ACmp(_) } | "!" ~> assn ^^ { ANot(_) } | aformular

    def aformular : Parser[Assertion] =
      ("forall" ~> rep(pvar)) ~ ("," ~> assn) ^^ {
        case xs ~ a => {
          if (xs.isEmpty) {
            a
          } else {
            def idsAssn = Helpers.getIdsAssn(a)
            def idsForall = xs.map({
              id => if (idsAssn.contains(IntId(id))) {
                IntId(id)
              } else if (idsAssn.contains(ArrId(id))) {
                ArrId(id)
              } else {
                DummyId(id)
              }
            })
            AForall(idsForall, a)
          }
        }
      } |
      ("exists" ~> rep(pvar)) ~ ("," ~> assn) ^^ {
        case xs ~ a => {
          if (xs.isEmpty) {
            a
          } else {
            def idsAssn = Helpers.getIdsAssn(a)
            def idsForall = xs.map({
              id => if (idsAssn.contains(IntId(id))) {
                IntId(id)
              } else if (idsAssn.contains(ArrId(id))) {
                ArrId(id)
              } else {
                DummyId(id)
              }
            })
            AForall(idsForall, a)
          }
        }
      }

    def aconj : Parser[Assertion] =
      aatom ~ rep("&&" ~> aatom) ^^ {
        case left ~ list => (left /: list) { AAnd(_, _) }
      }

    def adisj : Parser[Assertion] =
      aconj ~ rep("||" ~> aconj) ^^ {
        case left ~ list => (left /: list) { AOr(_, _) }
      }

    def aimply : Parser[Assertion] =
      adisj ~ ("==>" ~> aimply) ^^ { case pre ~ post => AImply(pre, post) } | adisj


    def assn : Parser[Assertion] = aimply

    /* Parsing for Program. */
    // def prog   : Parser[Program] =
    //   ("program" ~> pvar <~ "is") ~ (block <~ "end") ^^ {
    //     case n ~ b => (n, b)
    //   }

    def prog : Parser[HoareProgram] =
      ("program" ~> pvar) ~ rep("pre" ~> assn) ~ rep("post" ~> assn) ~ ("is" ~> block <~ "end") ^^ { case x ~ pres ~ posts ~ b => (x, pres, posts, b) }
  }

  object PrettyPrinter {

    /* pretty-print a program */
    def progPrint(prog: HoareProgram): Unit = prog match {
      case (x, pres, posts, b) => {
        println(x)
        for (pre <- pres) {
          print("pre: ")
          assnPrint(pre)
          println()
        }
        for (post <- posts) {
          print("post: ")
          assnPrint(post)
          println()
        }
        println(b)
      }
    }

    /* pretty-print an assertion */
    /* basically, if the sub-exp has lower precedence, print a pair of parens */
    def assnPrint(assn: Assertion): Unit = assn match {
      case ACmp(cmp) => compPrint(cmp)
      case ANot(a) => {
        print("!")
        assnPrint(a)
      }
      case AAnd(left, right) => {
        if (Helpers.isApply(left)) {
          print("(")
          assnPrint(left)
          print(")")
        } else {
          assnPrint(left)
        }
        print(" && ")
        if (Helpers.isApply(right)) {
          print("(")
          assnPrint(right)
          print(")")
        } else {
          assnPrint(right)
        }
      }
      case AOr(left, right) => {
        assnPrint(left)
        print(" || ")
        assnPrint(right)
      }
      case AImply(pre, post) => {
        assnPrint(pre)
        print(" ==> ")
        assnPrint(post)
      }
      case AForall(xs, a) => {
        println()
        print("forall ")
        for (x <- xs) {
          print(x)
          print(" ")
        }
        print(", ")
        assnPrint(a)
        println()
      }
      case AExist(xs, a) => {
        println()
        print("exists ")
        for (x <- xs) {
          print(x)
          print(" ")
        }
        print(", ")
        assnPrint(a)
        println()
      }
      case AParen(a) => {
        print("(")
        assnPrint(a)
        print(")")
      }
      case ATrue() => {
        print("True")
      }
    }

    /* pretty-print a comparison */
    def compPrint(cmp: Comparison): Unit = cmp match {
      case (left, op, right) => {
        arithPrint(left)
        print(op)
        arithPrint(right)
      }
    }

    /* pretty-print a arith */
    def arithPrint(aexp: ArithExp): Unit = aexp match {
      case Num(n) => print(n)
      case Var(x) => print(x)
      case Add(l, r) => {
        arithPrint(l)
        print("+")
        arithPrint(r)
      }
      case Sub(l, r) => {
        arithPrint(l)
        print("-")
        arithPrint(r)
      }
      case Mul(l, r) => {
        arithPrint(l)
        print("*")
        arithPrint(r)
      }
      case Div(l, r) => {
        arithPrint(l)
        print("/")
        arithPrint(r)
      }
      case Mod(l, r) => {
        arithPrint(l)
        print("%")
        arithPrint(r)
      }
      case Parens(a) => {
        print("(")
        arithPrint(a)
        print(")")
      }
      case Read(arr, ind) => {
        arrPrint(arr)
        print("[")
        arithPrint(ind)
        print("]")
      }
    }

    /* pretty-print an arry */
    def arrPrint(arr: Arr): Unit = arr match {
      case Varr(name) => print(name)
      case Warr(arr, ind, value) => {
        arrPrint(arr)
        print("[")
        arithPrint(ind)
        print("->")
        arithPrint(value)
        print("]")
      }
    }
  }

  def atrue: Assertion = ATrue()

  object VCGenerator {
    /* calculate the weakest condition */
    def preCond(program: Block, assn: Assertion): Assertion = program match {
      case Nil => assn
      case (stmt: Statement) :: Nil => stmt match {
        case Assign(x, e) => substAssn(x, e)(assn)
        case ParAssign(x1, x2, e1, e2) => substAssnPar(x1, x2, e1, e2)(assn)
        case Write(arr, ind, v) => substAssnArr(arr, Warr(Varr(arr), ind, v))(assn)
        case ParWrite(arr1, ind1, arr2, ind2, v1, v2) => {
          if (arr1 == arr2) {
            substAssnArr(arr1, Warr(Warr((Varr(arr1)), ind1, v1), ind2, v2))(assn)
          } else {
            substAssnArrPar(arr1, Warr(Varr(arr1), ind1, v1), arr2, Warr(Varr(arr2), ind2, v2))(assn)
          }
        }
        case If(cond, th, el) => {
          def condA = boolToAssn(cond)
          def preTh = AImply(condA, preCond(th, assn))
          def preEl = AImply(ANot(condA), preCond(el, assn))
          return AAnd(preTh, preEl)
        }
        case WhileInv(cond, block, inv) => {
          def invs = (atrue /: inv) { AAnd(_, _) }
          def condA = boolToAssn(cond)
          def vars = Helpers.getModifiedIds(block).toList
          def invHold = AForall(vars, AImply(AAnd(invs, condA), preCond(block, invs)))
          def invImply = AForall(vars, AImply(AAnd(invs, ANot(condA)), assn))
          return AAnd(invs, (AAnd(invHold, invImply)))
        }
      }
      case x :: xs => {
        preCond(x::Nil, preCond(xs, assn))
      }
    }

    /* boolean expression and assertion should be isomorphic */
    def boolToAssn(b: BoolExp): Assertion = b match {
      case BCmp(cmp) => ACmp(cmp)
      case BNot(b) => ANot(boolToAssn(b))
      case BDisj(left, right) => {
        def lefta = boolToAssn(left)
        def righta = boolToAssn(right)
        return AOr(lefta, righta)
      }
      case BConj(left, right) => {
        def lefta = boolToAssn(left)
        def righta = boolToAssn(right)
        return AAnd(lefta, righta)
      }
      case BParens(b) => AParen(boolToAssn(b))
    }

    /* helper function: walk through the assertion ADT (especially for subst)*/
    def assnWalk(assn: Assertion,
      fcomp: Comparison => Comparison,
      fassn: Assertion => Assertion,
      fformula: List[TypedId] => Assertion => Assertion): Assertion = assn match {
      case ACmp(cmp) => ACmp(fcomp(cmp))
      case ANot(a) => ANot(fassn(a))
      case AAnd(l, r) => AAnd(fassn(l), fassn(r))
      case AOr(l, r) => AOr(fassn(l), fassn(r))
      case AImply(l, r) => AImply(fassn(l), fassn(r))
      case AForall(xs, a) => AForall(xs, fformula(xs)(a))
      case AExist(xs, a) => AExist(xs, fformula(xs)(a))
      case AParen(a) => AParen(fassn(a))
      case ATrue() => ATrue()
    }

    /* substitute e for x in assertion*/
    def substAssn(x: String, e: ArithExp): Assertion => Assertion = {
      def fcomp: Comparison => Comparison = { case (l, op, r) => (substArith(x, e)(l), op, substArith(x, e)(r))}
      def fassn = substAssn(x, e)
      def fformula: List[TypedId] => Assertion => Assertion = {
        xs => if (xs.contains(IntId(x))) assn => assn else substAssn(x, e)
      }
      return { assn => assnWalk(assn, fcomp, fassn, fformula) }
    }

    /* substitute e1 for x1 and e2 for x2 simultaneously in assertion */
    def substAssnPar(x1: String, x2: String,
      e1: ArithExp, e2: ArithExp): Assertion => Assertion = {
      def fcomp: Comparison => Comparison = {
        case (l, op, r) => (substArithPar(x1, x2, e1, e2)(l), op, substArithPar(x1, x2, e1, e2)(r))
      }
      def fassn = substAssnPar(x1, x2, e1, e2)
      def fformula: List[TypedId] => Assertion => Assertion = {
        xs => if (xs.contains(IntId(x1)) && xs.contains(IntId(x1))) {
          assn => assn
        } else if (xs.contains(IntId(x1))) {
          substAssn(x2, e2)
        } else if (xs.contains(IntId(x2))) {
          substAssn(x1, e1)
        } else {
          substAssnPar(x1, x2, e1, e2)
        }
      }
      return { assn => assnWalk(assn, fcomp, fassn, fformula) }
    }

    /* substitute newArr for oldArr in assertion */
    def substAssnArr(oldArr: String, newArr: Arr): Assertion => Assertion = {
      def fcomp: Comparison => Comparison = {
        case (l, op, r) => (substArithArr(oldArr, newArr)(l), op, substArithArr(oldArr, newArr)(r))
      }
      def fassn = substAssnArr(oldArr, newArr)
      def fformula: List[TypedId] => Assertion => Assertion = {
        xs => if (xs.contains(ArrId(oldArr))) {
          assn => assn
        } else {
          substAssnArr(oldArr, newArr)
        }
      }
      return { assn => assnWalk(assn, fcomp, fassn, fformula) }
    }

    def substAssnArrPar(o1: String, n1: Arr, o2: String, n2: Arr): Assertion => Assertion = {
      def faux = substArithArrPar(o1, n1, o2, n2)
      def fcomp: Comparison => Comparison = {
        case (l, op, r) => (faux(l), op, faux(r))
      }
      def fassn = substAssnArrPar(o1, n1, o2, n2)
      def fformula: List[TypedId] => Assertion => Assertion = {
        xs => if (xs.contains(ArrId(o1)) && xs.contains(ArrId(o2))) {
          assn => assn
        } else if (xs.contains(ArrId(o1))) {
          substAssnArr(o2, n2)
        } else if (xs.contains(ArrId(o2))) {
          substAssnArr(o1, n1)
        } else {
          substAssnArrPar(o1, n1, o2, n2)
        }
      }
      return { assn => assnWalk(assn, fcomp, fassn, fformula) }
    }

    /* helper function walk through the arithExp ADT */
    def arithWalk(aexp: ArithExp, fn: Int => Int,
      fvar: String => ArithExp,
      fexp: ArithExp => ArithExp,
      farr: Arr => Arr): ArithExp = aexp match {
      case Num(n) => Num(fn(n))
      case Var(x) => fvar(x)
      case Add(l, r) => Add(fexp(l), fexp(r))
      case Sub(l, r) => Sub(fexp(l), fexp(r))
      case Mul(l, r) => Mul(fexp(l), fexp(r))
      case Div(l, r) => Div(fexp(l), fexp(r))
      case Mod(l, r) => Mod(fexp(l), fexp(r))
      case Parens(a) => Parens(fexp(a))
      case Read(arr, ind) => Read(farr(arr), fexp(ind))
    }

    /* substitute e for x in arithExp */
    def substArith(x: String, e: ArithExp): ArithExp => ArithExp = {
      def fn: Int => Int = n => n
      def fvar: String => ArithExp = { y => if (x == y) e else Var(y) }
      def fexp = substArith(x, e)
      def farr = substArr(x, e)
      return {aexp => arithWalk(aexp, fn, fvar, fexp, farr) }
    }

    /*
     what is the semantics for:
     x, x := y, z
     */
    /* substitute e1 for x1 and e2 for x2 simultaneously in arithExp */
    def substArithPar(x1: String, x2: String,
      e1: ArithExp, e2: ArithExp): ArithExp => ArithExp = {
      def fn: Int => Int = n => n
      def fvar: String => ArithExp = {
        x => if (x == x1) {
          e1
        } else if (x == x2) {
          e2
        } else {
          Var(x)
        }
      }
      def fexp = substArithPar(x1, x2, e1, e2)
      def farr = substArrPar(x1, x2, e1, e2)
      return {aexp => arithWalk(aexp, fn, fvar, fexp, farr) }
    }

    /* substitute newArr for oldArr in arithExp */
    def substArithArr(oldArr: String, newArr: Arr): ArithExp => ArithExp = {
      def fn: Int => Int = n => n
      def fvar: String => ArithExp = { x => Var(x) }
      def fexp = substArithArr(oldArr, newArr)
      def farr: Arr => Arr = substArrArr(oldArr, newArr)
      return {aexp => arithWalk(aexp, fn, fvar, fexp, farr) }
    }

    def substArithArrPar(o1: String, n1: Arr, o2: String, n2: Arr): ArithExp => ArithExp = {
      def fn: Int => Int = n => n
      def fvar: String => ArithExp = {x => Var(x) }
      def fexp = substArithArrPar(o1, n1, o2, n2)
      def farr: Arr => Arr = substArrArrPar(o1, n1, o2, n2)
      return { aexp => arithWalk(aexp, fn, fvar, fexp, farr) }
    }

    /* substitute e for x in Arr */
    def substArr(x: String, e: ArithExp): Arr => Arr = {
      case Varr(name) => Varr(name)
      case Warr(warr, ind, value) => {
        def warrs = substArr(x, e)(warr)
        def inds = substArith(x, e)(ind)
        def values = substArith(x, e)(value)
        Warr(warrs, inds, values)
      }
    }

    /* substitute e1 for x1 and e2 for x2 simultaneously in Arr */
    def substArrPar(x1: String, x2: String, e1: ArithExp, e2: ArithExp): Arr => Arr = {
      case Varr(name) => Varr(name)
      case Warr(warr, ind, value) => {
        def warrs = substArrPar(x1, x2, e1, e2)(warr)
        def inds = substArithPar(x1, x2, e1, e2)(ind)
        def values = substArithPar(x1, x2, e1, e2)(value)
        Warr(warrs, inds, values)
      }
    }

    /* substitute newArr for oldArr in arr */
    def substArrArr(oldArr: String, newArr: Arr): Arr => Arr = {
      case Varr(name) => if (name == oldArr) newArr else Varr(name)
      case Warr(warr, ind, value) => {
        def warrs = substArrArr(oldArr, newArr)(warr)
        def inds = substArithArr(oldArr, newArr)(ind)
        def values = substArithArr(oldArr, newArr)(value)
        Warr(warrs, inds, values)
      }
    }

    /*
     Note: a[i],a[i+1] := a[i+1],a[i] and a[i],b[i] := b[i],a[i] are dealt differently.
     */
    def substArrArrPar(o1: String, n1: Arr, o2: String, n2: Arr): Arr => Arr = {
      case Varr(name) => {
        if (name == o1) {
          n1
        } else if (name == o2) {
          n2
        } else {
          Varr(name)
        }
      }
      case Warr(warr, ind, value) => {
        def warrs = substArrArrPar(o1, n1, o2, n2)(warr)
        def inds = substArithArrPar(o1, n1, o2, n2)(ind)
        def values = substArithArrPar(o1, n1, o2, n2)(value)
        Warr(warrs, inds, values)
      }
    }

    def preCondProgram(program: HoareProgram): Assertion = program match {
      case (name, pres, posts, body) => {
        def pre = (atrue /: pres) { AAnd(_, _) }
        def post = (atrue /: posts) { AAnd(_, _) }
        AImply(pre, preCond(body, post))
      }
    }

  }

  object SMTFormat {
    def assnToSMT(assn: Assertion): String = assn match {
      case ACmp(cmp) => cmpToSMT(cmp)
      case ANot(a) => "(" + "not " + assnToSMT(a) + ")"
      case AAnd(left, right) => "(and " + assnToSMT(left) + " " + assnToSMT(right) + ")"
      case AOr(left, right) => "(or " + assnToSMT(left) + " " + assnToSMT(right) + ")"
      case AImply(pre, post) => "(=> " + assnToSMT(pre) + " " + assnToSMT(post) + ")"
      case AForall(xs, a) => "(forall (" + xs.flatMap({
        case IntId(x) => "(" + x + " Int) "
        case ArrId(x) => "(" + x + " (Array Int Int)) "
        case DummyId(x) => "(" + x + " (Array Int Int))"
      }).mkString + ") " + assnToSMT(a) + ")"
      case AExist(xs, a) => "(exists (" + xs.flatMap("(" + _ + " Int) ").mkString + ") " + assnToSMT(a) + ")"
      case AParen(a) => assnToSMT(a)
      case ATrue() => "true" // should not be called
    }

    def cmpToSMT(cmp: Comparison): String = cmp match {
      case (left, op, right) => {
        if (op == "!=") {
          "(not " + cmpToSMT((left, "=", right)) + ")"
        } else {
          "(" + op + " " + arithToSMT(left) + " " + arithToSMT(right) + ")"
        }
      }
    }

    def arithToSMT(aexp: ArithExp): String = aexp match {
      case Num(n) => n.toString
      case Var(x) => x
      case Add(left, right) => "(+ " + arithToSMT(left) + " " + arithToSMT(right) + ")"
      case Sub(left, right) => "(- " + arithToSMT(left) + " " + arithToSMT(right) + ")"
      case Mul(left, right) => "(* " + arithToSMT(left) + " " + arithToSMT(right) + ")"
      case Div(left, right) => "(div " + arithToSMT(left) + " " + arithToSMT(right) + ")"
      case Mod(left, right) => "(mod " + arithToSMT(left) + " " + arithToSMT(right) + ")"
      case Parens(a) => arithToSMT(a)
      case Read(arr, ind) => "(select " + arrToSMT(arr) + " " + arithToSMT(ind) + ")"
    }

    def arrToSMT(arr: Arr): String = arr match {
      case Varr(name) => name
      case Warr(arr, ind, value) => "(store " + arrToSMT(arr) + " " + arithToSMT(ind) + " " + arithToSMT(value) + ")"
    }

    def constDeclare(xs: Set[TypedId]): String = {
      xs.toList.flatMap({
        case IntId(x) => "(declare-const " + x + " Int)\n"
        case ArrId(x) => "(declare-const " + x + " (Array Int Int))\n"
        case DummyId(x) => {
          println("dummy type id!: " + x)
          return ("(declare-const " + x + " Int)\n")
        }
      }).mkString
    }
  }

  object Helpers {
    def getModifiedIds(program: Block): Set[TypedId] = program match {
      case Nil => Set()
      case stmt :: remain => {
        def ids = stmt match {
          case Assign(x, _) => Set(IntId(x))
          case ParAssign(x1, x2, _, _) => Set(IntId(x1), IntId(x2))
          case Write(x, _, _) => Set(ArrId(x))
          case ParWrite(x1, _, x2, _, _, _) => Set(ArrId(x1), ArrId(x2))
          case If(_, th, el) => getModifiedIds(th) ++ getModifiedIds(el)
          case WhileInv(_, block, _) => getModifiedIds(block)
          case While(_, block) => getModifiedIds(block)
        }
        return ids ++ getModifiedIds(remain)
      }
    }

    def getIdsAssn(assn: Assertion): Set[TypedId] = assn match {
      case ACmp((left, op, right)) => getIdsArith(left) ++ getIdsArith(right)
      case ANot(a) => getIdsAssn(a)
      case AParen(a) => getIdsAssn(a)
      case AAnd(left, right) => getIdsAssn(left) ++ getIdsAssn(right)
      case AOr(left, right) => getIdsAssn(left) ++ getIdsAssn(right)
      case AImply(left, right) => getIdsAssn(left) ++ getIdsAssn(right)
      case ATrue() => Set()
      case AForall(xs, a) => getIdsAssn(a) -- xs.toSet
      case AExist(xs, a) => getIdsAssn(a) -- xs.toSet
    }

    def getIdsArith(aexp: ArithExp): Set[TypedId] = aexp match {
      case Num(n) => Set()
      case Var(x) => Set(IntId(x))
      case Add(l, r) => getIdsArith(l) ++ getIdsArith(r)
      case Sub(l, r) => getIdsArith(l) ++ getIdsArith(r)
      case Mul(l, r) => getIdsArith(l) ++ getIdsArith(r)
      case Div(l, r) => getIdsArith(l) ++ getIdsArith(r)
      case Mod(l, r) => getIdsArith(l) ++ getIdsArith(r)
      case Parens(a) => getIdsArith(a)
      case Read(arr, ind) => getIdsArr(arr) ++ getIdsArith(ind)
    }

    def getIdsArr(arr: Arr): Set[TypedId] = arr match {
      case Varr(name) => Set(ArrId(name))
      case Warr(arr, ind, value) =>
        getIdsArr(arr) ++ getIdsArith(ind) ++ getIdsArith(value)
    }

    def isTrue(assn: Assertion): Boolean = assn match {
      case ACmp(_) | ANot(_) => false
      case AAnd(l, r) => isTrue(l) && isTrue(r)
      case AOr(l, r) => isTrue(l) || isTrue(r)
      case AParen(a) => isTrue(a)
      case AImply(pre, post) => isTrue(post)
      case ATrue() => true
      case AForall(_, a) => isTrue(a)
      case AExist(_, a) => isTrue(a)
    }

    def isApply(assn: Assertion): Boolean = assn match {
      case AImply(_, _) => true
      case _ => false
    }

    def removeTrue(assn: Assertion): Assertion = assn match {
      case AAnd(l, r) => {
        if (isTrue(l)) {
          removeTrue(r)
        } else if (isTrue(r)) {
          removeTrue(l)
        } else {
          AAnd(removeTrue(l), removeTrue(r))
        }
      }
      case AImply(pre, post) => AImply(removeTrue(pre), removeTrue(post))
      case AForall(xs, a) => AForall(xs, removeTrue(a))
      case AExist(xs, a) => AExist(xs, removeTrue(a))
      case _ => assn
    }

  }

  def main(args: Array[String]): Unit = {
    val reader = new FileReader(args(0))
    import ImpParser._;
    import VCGenerator._;
    import Helpers._;
    import SMTFormat._;
    val r = parseAll(prog, reader)
    if (r.successful) {
      val p = r.get
      val pre = removeTrue(preCondProgram(p))
      // println("pre:")
      // assnPrint(pre)
      // println()
      // println("SMT:")
      val assnPre = ANot(pre)
      val smtPre = assnToSMT(assnPre)
      val ids = getIdsAssn(pre)
      val declaration = constDeclare(ids)
      println(declaration)
      println("(assert ")
      println(smtPre)
      println(")\n(check-sat)\n(get-model)")
    } else {
      println(r)
    }
    //    println(parseAll(prog, reader))
  }
}
