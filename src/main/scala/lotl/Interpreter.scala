package lotl

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import AST._

trait InterpreterTypes {
  case class Env(locals: Map[Symbol, Expr], functions: Map[Symbol, PureForm],
                 pureForms: Map[Symbol, PureForm], forms: Map[Symbol, Form]) {
    def withLocal(newLocal: (Symbol, Expr)): Env = Env(locals + newLocal, functions, pureForms, forms)
    def withLocals(newLocals: Seq[(Symbol, Expr)]): Env = Env(locals ++ newLocals, functions, pureForms, forms)
    def withFunction(newFunction: (Symbol, PureForm)): Env = Env(locals, functions + newFunction, pureForms, forms)
    def withFunctions(newFunctions: Seq[(Symbol, PureForm)]): Env = Env(locals, functions ++ newFunctions, pureForms, forms)
  }

  type =>?[-A, +B] = PartialFunction[A, B]

  type Result[+A] = \/[String, A]
  // Env => \/[String, A]]
  type PureEval[A] = ReaderT[Result, Env, A]
  // Env => \/[String, (Env, A])]
  type Eval[A] = StateT[Result, Env, A]

  // Expr =>? Env => \/[String, Expr]]
  type PureForm = Expr =>? PureEval[Expr]
  // Expr =>? Env => \/[String, (Env, Expr)]
  type Form = Expr =>? Eval[Expr]

  object Env {
    val empty = new Env(Map.empty, Map.empty, Map.empty, Map.empty)
  }

  object Error {
    def lift[A](option: Option[A], message: => String): Result[A] = option match {
      case Some(x) => x.right
      case None => message.left
    }
  }

  object PureEval {
    def apply[A](f: Env => Result[A]): PureEval[A] = Kleisli.apply[Result, Env, A](f)
    def lift[A](a: A): PureEval[A] = apply(x => a.right)
    def liftR[A](u: Result[A]): PureEval[A] = apply(x => u)
  }

  object Eval {
    def apply[A](f: Env => Result[(Env, A)]): Eval[A] = StateT.apply[Result, Env, A](f)
  }

  object PureForm {
    def lift(f: Expr =>? Expr): PureForm = new =>?[Expr, PureEval[Expr]] {
      override def isDefinedAt(x: Expr): Boolean = f.isDefinedAt(x)
      override def apply(v: Expr): PureEval[Expr] = PureEval { e => f(v).right}
    }

    def liftR(f: Expr =>? Result[Expr]): PureForm = new =>?[Expr, PureEval[Expr]] {
      override def isDefinedAt(x: Expr): Boolean = f.isDefinedAt(x)
      override def apply(v: Expr): PureEval[Expr] = PureEval { e => f(v)}
    }
  }
}

trait BasicForms extends InterpreterTypes {
  val quote: PureForm = PureForm.lift { case ConsList(x) => x }
  val cond: PureForm = {
    case PairList(branches @_*) => PureEval { env =>
      @tailrec def cond0(branches: Seq[(AST.Expr, AST.Expr)]): Result[AST.Expr] = branches match {
        case (condition, value) +: tail => eval(condition)(env) match {
          case error @ -\/(_) => error
          case \/-(TrueAtom) => eval(value)(env)
          case \/-(NilAtom) => cond0(tail)
          case \/-(x) => "conditions should evaluate to T or NIL".left
        }
        case Nil => "unsatisfied conditional expression".left
      }

      cond0(branches)
    }
  }
  val defun: Form = {
    case ConsList(Atom(name), SymbolList(paramNames @_*), body) => Eval { env =>
      val func: PureForm = {
        case ConsList(params @_*) if params.size == paramNames.size => PureEval { env =>
          eval(body)(env.withLocals(paramNames.zip(params)))
        }
      }
      (env.withFunction(name -> func), name).right
    }
  }

  val eval: PureForm = {
    case value@TrueAtom => PureEval.lift(value)
    case value@NilAtom => PureEval.lift(value)
    case value@Integer(_) => PureEval.lift(value)

    case Atom(name) => PureEval(env => Error.lift(env.locals.get(name), s"unbound variable $name"))

    case Cons(Atom(name), argList @ ConsList(unevaluated @_*)) => PureEval { env =>
      env.pureForms.get(name).map { form =>
        if (form.isDefinedAt(argList)) form(argList)(env)
        else s"invalid arguments for form $name".left
      } orElse {
        env.functions.get(name).map { form =>
          val evaluated: List[Result[Expr]] = unevaluated.map {
            eval(_)(env)
          }.toList
          val sequenced: Result[List[Expr]] = evaluated.sequence
          sequenced.flatMap { p =>
            val evaluatedArgList = ConsList(p: _*)
            if (form.isDefinedAt(evaluatedArgList)) form(evaluatedArgList)(env)
            else s"invalid arguments for function $name".left
          }
        }
      } match {
        case Some(x) => x
        case None => s"unbound function or form $name".left
      }
    }

    case x => PureEval.liftR(s"unknown expression".left)
  }
  val topEval: Form = {
    case input @ Cons(Atom(name), argList @ ConsList(_*)) => Eval { env =>
      env.forms.get(name).map { form =>
        if (form.isDefinedAt(argList)) form(argList)(env)
        else s"invalid arguments for form $name".left
      } match {
        case Some(x) => x
        case None => eval(input).state.run(env)
      }
    }
    case x => eval(x).state
  }
}

trait BasicFunctions extends InterpreterTypes {
  val car = PureForm.lift { case ConsList(Cons(x, y)) => x }
  val cdr = PureForm.lift { case ConsList(Cons(x, y)) => y }
  val cons = PureForm.lift { case ConsList(x, y) => Cons(x, y) }
  val atom = PureForm.lift {
    case ConsList(Atom(_)) => TrueAtom
    case ConsList(Integer(_)) => TrueAtom
    case ConsList(_) => NilAtom
  }
  val isNull = PureForm.lift {
    case ConsList(NilAtom) => TrueAtom
    case ConsList(_) => NilAtom
  }
  val int = PureForm.lift {
    case ConsList(Integer(_)) => TrueAtom
    case ConsList(_) => NilAtom
  }
  val eq = PureForm.lift {
    case ConsList(NilAtom, NilAtom) => TrueAtom
    case ConsList(Atom(x), Atom(y)) if x == y => TrueAtom
    case ConsList(Integer(x), Integer(y)) if x == y => TrueAtom
    case ConsList(_, _) => NilAtom
  }
  val plus = PureForm.lift { case ConsList(Integer(x), Integer(y)) => Integer(x + y) }
  val minus = PureForm.lift { case ConsList(Integer(x), Integer(y)) => Integer(x - y) }
  val times = PureForm.lift { case ConsList(Integer(x), Integer(y)) => Integer(x * y) }
  val quotient = PureForm.lift { case ConsList(Integer(x), Integer(y)) => Integer(x / y) }
  val remainder = PureForm.lift { case ConsList(Integer(x), Integer(y)) => Integer(x % y) }
  val less = PureForm.lift { case ConsList(Integer(x), Integer(y)) => if (x < y) TrueAtom else NilAtom }
  val greater = PureForm.lift { case ConsList(Integer(x), Integer(y)) => if (x > y) TrueAtom else NilAtom }
}

object Interpreter extends BasicForms with BasicFunctions {
  def clear: Form = {
    case NilAtom => Eval { env => (prelude, TrueAtom).right }
  }

  def dumpenv: PureForm = {
    case NilAtom => PureEval { env =>
      PairList(
        Atom('locals) -> ConsList(env.locals.keys.map(Atom).toSeq :_*),
        Atom('functions) -> ConsList(env.functions.keys.map(Atom).toSeq :_*),
        Atom('pureForms) -> ConsList(env.pureForms.keys.map(Atom).toSeq :_*),
        Atom('forms) -> ConsList(env.forms.keys.map(Atom).toSeq :_*)
      ).right
    }
  }

  val prelude = Env(
    Map.empty,
    Map(
      'CAR -> car, 'CDR -> cdr, 'CONS -> cons,
      'ATOM -> atom, 'NULL -> isNull, 'INT -> int, 'EQ -> eq,
      'PLUS -> plus, 'MINUS -> minus, 'TIMES -> times, 'QUOTIENT -> quotient, 'REMAINDER -> remainder,
      'LESS -> less, 'GREATER -> greater
    ),
    Map('QUOTE -> quote, 'COND -> cond, 'DUMPENV -> dumpenv),
    Map('DEFUN -> defun, 'CLEAR -> clear)
  )
}