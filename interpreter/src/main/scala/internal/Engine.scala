package scala.reflect.interpreter
package internal

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.runtime.AbstractFunction1

abstract class Engine extends InterpreterRequires with Definitions with Errors with Emulators {
  import u._
  import definitions._
  import internal.decorators._

  def eval(tree: Tree): Any = {
    // can only interpret fully attributes trees
    // which is why we can't test the interpreter on the output of reify
    // TODO: in Palladium this will become irrelevant, because all trees
    // are going to have a `Tree.tpe` method that will actually typecheck stuff if necessary
    tree.foreach {
      case Ident(termNames.WILDCARD)     =>
      case sub if sub.tpe == null        => UnattributedAst(sub)
      case sub if sub.symbol == NoSymbol => UnattributedAst(sub)
      case _                             =>
    }
    val initialEnv = Env(List(ListMap()), ListMap())
    val (value, finalEnv) = eval(tree, initialEnv)
    val (result, _) = value.reify(finalEnv)
    result
  }

  def eval(tree: Tree, env: Env): Result = tree match {
    case q"${value: Value}"                   => (value, env)
    case EmptyTree                            => eval(q"()", env) // when used in blocks, means "skip that tree", so we evaluate to whatever
    case Literal(_)                           => evalLiteral(tree, env)
    case New(_)                               => Value.instantiate(tree.tpe, env)
    case Ident(_)                             => env.lookup(tree.symbol) // q"$_" would've matched any tree, not just an ident
    case s: Super                             => eval(s.qual, env)
    case q"$qual.$_"                          => evalSelect(qual, tree.symbol, env)
    case q"$_.this"                           => env.lookup(tree.symbol)
    case q"$expr.isInstanceOf[$tpt]"          => evalTypeTest(expr, tpt.tpe, env)
    case q"$expr.asInstanceOf[$tpt]"          => evalTypeCast(expr, tpt.tpe, env)
    case Apply(expr, args)                    => evalApply(expr, args, env) // the q"$expr[..$targs](...$argss)" quasiquote is too high-level for this
    case TypeApply(expr, targs)               => eval(expr, env)
    case q"$lhs = $rhs"                       => evalAssign(lhs, rhs, env)
    case q"return $expr"                      => evalReturn(expr, env)
    case q"throw $expr"                       => evalThrow(expr, env)
    case q"$expr: $_"                         => eval(expr, env)
    // case q"(..$exprs)"                     => never going to happen, because parser desugars these trees into applications
    case q"if ($cond) $then1 else $else1"     => evalIf(cond, then1, else1, env)
    case q"$scrut match { case ..$cases }"    => evalMatch(scrut, cases, env)
    case q"try $expr catch { case ..$cases } finally $finally1" => evalTryCatch(expr, cases, finally1, env)
    case q"(..$params) => $body"              => Value.function(params, body, env)
    // case q"{ case ..$cases }"              => never going to happen, because typer desugars these trees into anonymous class instantiations
    case q"while ($cond) $body"               => evalWhile(cond, body, env)
    case q"do $body while ($cond)"            => evalDoWhile(cond, body, env)
    case _: Block                             => evalBlock(tree.children, env) // qq feature workaround(lazy value wrapper functions skipped in stats)
    // case q"for (..$enums) $expr"           => never going to happen, because parser desugars these trees into applications
    // case q"for (..$enums) yield $expr"     => never going to happen, because parser desugars these trees into applications
    // case q"new { ..$early } with ..$parents { $self => ..$stats }" => never going to happen in general case, desugared into selects/applications of New
    case _: ValDef | _: DefDef | _: ModuleDef => evalLocalDef(tree, env) // can't skip these trees, because we need to enter them in scope when interpreting
    case _: MemberDef                         => eval(q"()", env) // skip these trees, because we have sym.source
    case _: Import                            => eval(q"()", env) // skip these trees, because it's irrelevant after typer, which has resolved all imports
    case _                                    => UnrecognizedAst(tree)
  }

  def eval(args: List[Tree], env: Env): Results = {
    args match {
      case arg :: tail =>
        val (varg, env1) = eval(arg, env)
        val (vtail, env2) = eval(tail, env1)
        (varg :: vtail, env2)
      case Nil =>
        (Nil, env)
    }
  }

  def evalLiteral(tree: Tree, env: Env): Result = {
    // this tedious pattern match is here to make sure that
    // we only support literal types that we are interested in
    // sure we could say `case Literal(Constant(x)) => reflect(x)`
    // but that would make evaluation less safe
    tree match {
      case q"${x: Byte}"                  => Value.reflect(x, env)
      case q"${x: Short}"                 => Value.reflect(x, env)
      case q"${x: Char}"                  => Value.reflect(x, env)
      case q"${x: Int}"                   => Value.reflect(x, env)
      case q"${x: Long}"                  => Value.reflect(x, env)
      case q"${x: Float}"                 => Value.reflect(x, env)
      case q"${x: Double}"                => Value.reflect(x, env)
      case q"${x: Boolean}"               => Value.reflect(x, env)
      case q"${x: Unit}"                  => Value.reflect(x, env)
      case q"${x: String}"                => Value.reflect(x, env)
      case q"null"                        => Value.reflect(null, env)
      case Literal(Constant(tpe: Type))   => RuntimeReflectionNotSupported(tree) // this tree shape stands for `classOf[$tpe]` after typechecking
      case Literal(Constant(sym: Symbol)) => UnrecognizedAst(tree) // this is a very obscure tree shape only used in annotations, and we don't eval those
      case _                              => UnrecognizedAst(tree)
    }
  }

  def defaultValue(tpe: Type, env: Env): Result = tpe match {
    case tpe if tpe == UnitTpe    => Value.reflect((), env)
    case tpe if tpe == BooleanTpe => Value.reflect(false, env)
    case tpe if tpe == FloatTpe   => Value.reflect(0.0f, env)
    case tpe if tpe == DoubleTpe  => Value.reflect(0.0d, env)
    case tpe if tpe == ByteTpe    => Value.reflect(0.toByte, env)
    case tpe if tpe == ShortTpe   => Value.reflect(0.toShort, env)
    case tpe if tpe == IntTpe     => Value.reflect(0, env)
    case tpe if tpe == LongTpe    => Value.reflect(0L, env)
    case tpe if tpe == CharTpe    => Value.reflect(0.toChar, env)
    case _                        => Value.reflect(null, env)
  }

  def evalLocalDef(tree: Tree, env: Env): Result = {
    val (vrepr, env1) = tree match {
      case ValDef(mods, _, tpt, rhs) =>
        // note how we bind ValDef's name to the default value of the corresponding type when evaluating the rhs
        // this is how Scala does it, so don't say that this looks weird :)
        val (vdefault, envx) = defaultValue(tpt.tpe, env)
        val (res, env1) = if (!mods.hasFlag(Flag.LAZY))
          eval(rhs, envx.extend(tree.symbol, vdefault))
        else
          (vdefault, envx)
        res.copy(env1)
      case tree: DefDef =>
        Value.method(tree.symbol.asMethod, env)
      case tree: ModuleDef =>
        Value.module(tree.symbol.asModule, env)
    }
    tree.symbol.owner match {
      case cd:ClassSymbol =>
        val (instance, env2) = env1.lookup(cd)
        (vrepr, env.extendHeap(env2).extend(instance, tree.symbol, vrepr))
      case cd:ModuleSymbol =>
        val (instance, env2) = env1.lookup(cd)
        (vrepr, env.extendHeap(env2).extend(instance, tree.symbol, vrepr))
      case _              =>
        (vrepr, env.extendHeap(env1).extend(tree.symbol, vrepr))
    }
  }

  def evalAssign(lhs: Tree, rhs: Tree, env: Env): Result = {
    // note that we don't need to process assignments that are desugared to `update` calls
    // these end up being processed in evalApply
    lhs match {
      case Ident(_) => // local value (things like `x` as in `this.x` have already been desugared by typer)
        val (vrhs, env1) = eval(rhs, env)
        val (vrhsc, env2) = vrhs.copy(env1)
        Value.reflect((), env2.extend(lhs.symbol, vrhsc))
      case Select(qual, _) => // someone's field
        val (vlhs, env1) = eval(qual, env)
        val (vrhs, env2) = eval(rhs, env1)
        Value.reflect((), env1.extendHeap(env2).extend(vlhs, lhs.symbol, vrhs))
    }
  }

  def evalBlock(stats: List[Tree], env: Env): Result = {
    // note how thanks to immutability of envs we don't have problems with local variable scoping
    // when evaluating a block, we can delegate introduction of locals to eval - it will update env and push the updates to us
    // when exiting a block, we just drop the local environment that we have accumulated without having to rollback anything
    val (_ :+ vstats, env1) = eval(stats, env)
    (vstats, env.extendHeap(env1))
  }

  def evalSelect(qual: Tree, sym: Symbol, env: Env): Result = {
    val (value, env2) = if (qual.symbol == null || !qual.symbol.isPackage) {
      val (vqual, env1) = eval(qual, env)
      vqual.select(sym, env1, static = qual.isInstanceOf[Super])
    } else {
      if (sym.isModule) Value.module(sym.asModule, env)
      else ??? // FIXME: other usecases of selection from package
    }
    value match {
      case f: CallableValue if f.isNullary => f.apply(Nil, env2)
      case _                               => (value, env2)
    }
  }

  def evalTypeTest(expr: Tree, tpe: Type, env: Env): Result = {
    val (vexpr, env1) = eval(expr, env)
    vexpr.typeTest(tpe, env1)
  }

  def evalTypeCast(expr: Tree, tpe: Type, env: Env): Result = {
    val (vexpr, env1) = eval(expr, env)
    vexpr.typeCast(tpe, env1)
  }

  def evalApply(expr: Tree, args: List[Tree], env: Env): Result = {
    // named and default args are already desugared by scalac, so we just perform straightforward evaluation
    // TODO: this will not be the case for palladium, but we'll see to that later
    // TODO: need to handle varargs (represented by q"arg: _*")
    val (vexpr, env1) = eval(expr, env)
    val (vargs, env2) = eval(args, env1)
    try {
      vexpr.apply(vargs, env2)
    } catch { case e@ReturnException(ret) => if (vexpr.isInstanceOf[MethodValue]) ret else throw e }
  }

  def evalIf(cond: Tree, then1: Tree, else1: Tree, env: Env): Result = {
    // handle reify side-effects in branches
    val (vcond, env1) = eval(cond, env)
    vcond.branch(env2 => eval(then1, env2), env2 => eval(else1, env2), env1)
  }

  @tailrec
  private def evalWhile(cond: Tree, body: Tree, env: Env): Result = {
    val (vcond, env1) = eval(cond, env)
    val (jcond, env2) = vcond.reify(env1)
    jcond match {
      case true =>
        val (_, env3) = eval(body, env2)
        evalWhile(cond, body, env3)
      case false =>
        Value.reflect((), env2)
      case _ =>
        IllegalState(jcond)
    }
  }

  private def evalDoWhile(cond: Tree, body: Tree, env: Env): Result = {
    val (_, bodyEnv) = eval(body, env)
    evalWhile(cond, body, bodyEnv)
  }

  case class ReturnException(value: Result) extends Exception
  case class WrappedException(ex: Result) extends Exception

  def evalReturn(expr: Tree, env: Env): Result = {
    val ret = eval(expr, env)
    throw ReturnException(ret)
  }

  def evalThrow(tree: Tree, env: Env): Result = {
    val res = eval(tree, env)
    throw WrappedException(res)
  }

  def evalTryCatch(tryBlock: Tree, cases: List[CaseDef], finally1: Tree, env: Env): Result = {
    try {
      eval(tryBlock, env)
    } catch {
      case WrappedException(excres) =>
        val (exceptionVal: Value, exceptionEnv) = excres
        evalMatch(q"$exceptionVal", cases, env.extendHeap(exceptionEnv).extend(NoSymbol, exceptionVal))
    }
  }

  def evalMatch(scrut: Tree, cases: List[CaseDef], env: Env): Result = {
    // TODO: semantics of evalPattern is just a sketch
    // it should be validated against the spec and the code that's emitted by the pattern matcher
    // TODO: also support name-based pattern matching
    // https://github.com/scala/scala/pull/2848
    def succeed(menv: Env, renv: Env) = eval(q"true", renv)
    def fail(menv: Env, renv: Env) = eval(q"false", renv)
    def evalPattern(vscrut: Value, pat: Tree, menv: Env, renv: Env): Result = {
      def checkCond(cond: Tree, onSuccess: (Env, Env) => Result = succeed, onFailure: (Env, Env) => Result = fail) = {
        val (vcond, menv1) = eval(cond, menv)
        vcond.branch(menv2 => onSuccess(menv2, renv.extendHeap(menv2)), menv2 => onFailure(menv2, renv.extendHeap(menv2)), menv1)
      }
      def checkPat(pat: Tree, onSuccess: (Env, Env) => Result = succeed, onFailure: (Env, Env) => Result = fail) = {
        val (vpat, renv1) = evalPattern(vscrut, pat, menv, renv)
        val menv1 = menv.extendHeap(renv1)
        vpat.branch(menv2 => onSuccess(menv2, renv1), menv2 => onFailure(menv2, renv1), menv1)
      }
      def checkEval(expr: Tree, cond: Value => Tree, result: Value => Tree, onSuccess: (Value, Env, Env) => Result) = {
        val (vexpr, menv1) = eval(expr, menv)
        val (vcond, menv2) = eval(cond(vexpr), menv1)
        def cont(menv3: Env) = {
          val (vresult, menv4) = eval(result(vexpr), menv3)
          onSuccess(vresult, menv4, renv.extendHeap(menv4))
        }
        vcond.branch(cont, menv3 => fail(menv3, renv.extendHeap(menv3)), menv2)
      }
      pat match {
        case Ident(termNames.WILDCARD) =>
          succeed(menv, renv)
        case Typed(_, tpt) =>
          checkCond(q"$vscrut.${Any_isInstanceOf}[$tpt]")
        case Bind(name, pat) =>
          checkPat(pat, onSuccess = (menv1, renv1) => succeed(menv1, renv1.extend(pat.symbol, vscrut)))
        case pat: Literal =>
          checkCond(q"$vscrut.${Any_equals}($pat)")
        case pat: RefTree =>
          checkCond(q"$vscrut.${Object_eq}($pat)")
        case Alternative(alt1 :: alt2 :: rest) =>
          val fallback = if (rest.isEmpty) alt2 else Alternative(alt2 :: rest)
          checkPat(alt1, onFailure = (menv1, renv1) => evalPattern(vscrut, fallback, menv1, renv1))
        case Apply(tpt, pats) => // can only happen when tpt is a case class
          def cont(menv1: Env, renv1: Env) = {
            val fields = tpt.tpe.decls.sorted.filter(sym => sym.isMethod && sym.asMethod.isCaseAccessor)
            val (vfields, matchEnv2) = eval(fields.map(f => q"$scrut.$f"), menv1)
            val renv1 = renv.extendHeap(matchEnv2)
            evalPatterns(vfields, pats, matchEnv2, renv1)
          }
          checkCond(q"$vscrut.${Any_isInstanceOf}[$tpt]", onSuccess = cont)
        case UnApply(Apply(unapply, List(dummy)), Nil) => // this is what other extractors are translated to
          checkCond(q"$unapply($vscrut)")
        case UnApply(Apply(unapply, List(dummy)), pats) =>
          def cont(result: Value, menv1: Env, renv1: Env) = {
            val fields = TupleClass(pats.length).info.decls.sorted.filter(sym => sym.isMethod && sym.asMethod.isCaseAccessor)
            val (vfields, menv2) = {
              if (pats.length == 1) (result :: Nil, menv1)
              else eval(fields.map(f => q"$scrut.$f"), menv1)
            }
            evalPatterns(vfields, pats, menv2, renv1.extendHeap(menv2))
          }
          checkEval(q"$unapply($vscrut)", vexpr => q"$vexpr.${Option_isDefined}", vexpr => q"$vexpr.${Option_get}", onSuccess = cont)
      }
    }
    def evalPatterns(vscruts: List[Value], pats: List[Tree], menv: Env, renv: Env): Result = (vscruts, pats) match {
      case (vscruts, Star(pat) :: Nil) =>
        // TODO: create a vararg array from values represented by vscruts and bind it to pat
        ???
      case (vscrut :: vrest, pat :: patrest) =>
        val (result, renv1) = evalPattern(vscrut, pat, menv, renv)
        result.branch(renv2 => evalPatterns(vrest, patrest, menv.extendHeap(renv2), renv2), renv2 => fail(menv.extendHeap(renv2), renv2), renv1)
      case (Nil, Nil) =>
        succeed(menv, renv)
    }
    def loop(vscrut: Value, cases: List[CaseDef], env: Env): Result = cases match {
      case CaseDef(pat, guard, body) :: rest =>
        val (vpat, env1) = evalPattern(vscrut, pat, env, env)
        val (vguard, env2) = vpat.branch(envb => eval(guard.orElse(q"true"), envb),
                                         envb => eval(q"false", envb), env1)
        vguard.branch(env3 => eval(body, env3), env3 => loop(vscrut, rest, env3), env2)
      case _ =>
        val (exn, env1) = Value.instantiate(typeOf[MatchError], env)
        val (exn1, env2) = exn.apply(List(vscrut), env1)
        ??? // TODO: throw exn1 in env2
    }
    val (vscrut, env1) = eval(scrut, env)
    loop(vscrut, cases, env1.extend(NoSymbol, vscrut)) // pass vscrut as "_" by NoSymbol(_ has none)
  }

  // can't make these classes final because of SI-4440
  sealed trait Slot
  case class Primitive(value: Any) extends Slot
  case class Object(fields: ListMap[Symbol, Value]) extends Slot
  type Heap = ListMap[Value, Slot]

  type FrameStack = List[ListMap[Symbol, Value]]

  case class Env(stack: FrameStack, heap: Heap) {
    def lookup(sym: Symbol): Result = {
      // all the stuff above might be effectful
      // therefore we return Result here, and not just Value
      // TODO: reflect java modules to call things like System.getenv() etc.
      val mod = if (sym.isModule) sym.asModule else if (sym.isClass && sym.isModuleClass) sym.asClass.module else sym
      mod match {
        case m: ModuleSymbol                     =>
          val (mod, env1) = stack.head.get(sym) match {
            case Some(value) => (value, this)
            case None        => Value.module(m, this)
          }
          val (res, env2) = mod.init(env1)
          (res, env2.extend(m, res))
        case _                                   =>
          try {
            stack.head.get(sym) match {
              case Some(f: CallableValue) if f.isNullary  => f.apply(Nil, this)
              case Some(other)                            => (other, this)
              case None                                   => IllegalState(sym)
            }
          } catch { case ReturnException(res) => res }
      }
    }
    def extend(sym: Symbol, value: Value): Env = {
      stack.head.get(sym) match {
        case Some(v: Value) if sym == NoSymbol => Env((stack.head + (sym -> value)) :: stack.tail, heap) // "_" reassigning workaround
        case Some(v: ReifiableValue)           => Env(stack, heap + (v -> heap(value)))
        case _                                 => Env((stack.head + (sym -> value)) :: stack.tail, heap)
      }
    }
    def extend(obj: Value, field: Symbol, value: Value): Env = {
      // extend heap with the new value for the given field of the given object
      heap(obj) match {
        case Object(fields) => extend(obj, Object(fields + (field -> value)))
        case _              => IllegalState(obj)
      }
    }
    def extend(obj: Value, newFields: Map[Symbol, Value]): Env = {
      heap(obj) match {
        case Object(fields) => extend(obj, Object(fields ++ newFields))
        case _              => IllegalState(obj)
      }
    }
    def extend(v: Value, s: Slot): Env = {
      Env(stack, heap + (v -> s))
    }
    def extendHeap(other: Env): Env = {
      // import heap from another environment
      Env(stack, heap ++ other.heap)
    }
    def extend(v: Value, a: Any): Env = {
      Env(stack, heap + (v -> Primitive(a)))
    }
    def pushFrame(other: Env) = {
      Env(other.stack.head +: stack, heap)
    }
  }

  @volatile private var _nextId = new java.util.concurrent.atomic.AtomicInteger()
  private def nextId() = _nextId.incrementAndGet()

  sealed trait Value extends MagicMethodEmulator {
    val id = nextId()
    def reify(env: Env): JvmResult = {
      // convert this interpreter value to a JVM value
      // return None if it refers to a not-yet-compiled class
      // note that it is probably possible to improve reify to work correctly in all cases
      // however this doesn't matter much for Project Palladium, so that's really low priority
      env.heap.get(this) match {
        case Some(Primitive(prim)) => (prim, env)
        case Some(Object(fields))  => (fields, env)
        case None                  => IllegalState(this)
      }
    }
    def select(member: Symbol, env: Env, static: Boolean = false): Result = {
      // note that we need env here, because selection might be effectful
      // also note that there's no need to evaluate empty-arglist methods here
      // if we have an empty-arglist application, then the result of select will be fed into apply(Nil)
      // needs to handle selections of field and method references
      // because e.g. foo.bar(1, 2) looks like Apply(Select(foo, bar), List(1, 2))
      if (member.isMethod) (selectCallable(this, member, env), env)
      else ???
    }
    def apply(args: List[Value], env: Env): Result = {
      // needs to work well both with functions and method references
      // also has to handle partial applications
      // when inside a method application, we should have enclosing this'es bound to corresponding class symbols
      // in the current model, constructors have to return the object being constructed
      ???
    }
    def branch[T](then1: Env => T, else1: Env => T, env: Env): T = {
      val (jvalue, env1) = reify(env)
      jvalue match {
        case true => then1(env1)
        case false => else1(env1)
        case other => IllegalState(other)
      }
    }
    def typeTest(tpe: Type, env: Env): Result = {
      // can't use a symbol here, because this can be an array. use tpe.erasure if in doubt
      if (tpt.typeSymbol == ArrayClass)
        if (tpt <:< tpe) Value.reflect(true, env) else Value.reflect(false, env)
      else
        if (tpt.erasure <:< tpe.erasure) Value.reflect(true, env) else Value.reflect(false, env)
    }
    def typeCast(tpe: Type, env: Env): Result = {
      // TODO: can't be a no-op, because we actually need to throw if the type is incompatible
      // TODO: also, we have to deal with all sort of conversions built into Scala: boxings, numeric stuff, nulls, etc
      ???
    }
    def copy(env: Env): Result = (this, env)
    def init(env: Env): Result = (this, env)
    val tpt: Type = typeOf[Any]
  }

  class TypedValue(override val tpt: Type) extends Value

  trait ReifiableValue

  class JvmValue(tpt: Type) extends TypedValue(tpt) with ReifiableValue {
    override def copy(env: Env) = {
      val v = new JvmValue(tpt)
      val (res, env1) = reify(env)
      (v, env1.extend(v, res))
    }
    override def toString = s"JvmValue#" + id
  }

  trait CallableValue extends Value {
    def isNullary: Boolean
  }

  case class FunctionValue(paramss: List[List[Symbol]], body: Tree, capturedEnv: Env) extends CallableValue {
    override def isNullary: Boolean = paramss.isEmpty
    override def apply(args: List[Value], callSiteEnv: Env): Result = {
      val env1 = callSiteEnv.pushFrame(capturedEnv)
      paramss match {
        case x :: Nil => // single parameter list - invoke
          val env2 = x.zip(args).foldLeft(env1)((tmpEnv, p) => tmpEnv.extend(p._1, p._2))
          val (res, env3) = eval(body, env2)
          (res, callSiteEnv.extendHeap(env3))
        case x :: xs => // multi parameter list - return curried function with N-1 parameter list
          val env2 = x.zip(args).foldLeft(env1)((tmpEnv, p) => tmpEnv.extend(p._1, p._2))
          (FunctionValue(xs, body, env2), callSiteEnv)
        case Nil =>      // empty paramlist - invoke without extending env
          val (res, env3) = eval(body, env1)
          (res, callSiteEnv.extendHeap(env3))
      }
    }

    override def select(member: Symbol, env: Env, static: Boolean = false): Result = {
      // for now we assume user cannot select methods other than apply from a function
      if (member.name.toString == "apply") (this, env) else ???
    }
    override def toString = s"FunctionValue#" + id
  }

  case class MethodValue(sym: MethodSymbol, capturedEnv: Env) extends CallableValue {
    override def apply(args: List[Value], callSiteEnv: Env): Result = {
      val src = source(sym).asInstanceOf[DefDef].rhs
      FunctionValue(sym.paramLists, src, capturedEnv.extend(sym, this)).apply(args, callSiteEnv)
    }
    override def isNullary: Boolean = sym.paramLists.isEmpty
    override def toString = s"MethodValue#" + id
  }

  class ObjectValue(sym: Symbol, tpe: Type) extends TypedValue(tpe) {
    val body = source(sym).asInstanceOf[ImplDef].impl.body
    class ObjectCtor(parent: ObjectValue, sym: MethodSymbol, capturedEnv: Env) extends MethodValue(sym, capturedEnv) {
      def extractConsArgs(tree: MemberDef, argss: List[List[ValDef]]): List[List[Symbol]] = {
        // workaround constructor argss symbol resolution
        // extract symbols from valdefs of class body by name
        // an extra whitespace is appended to name in valdef symbol
        val valDefs = body.collect({case d: ValDef => d}).map(it=>(it.name.toString.trim, it)).toMap
        argss.map(args => args.map(arg => valDefs(arg.name.toString).symbol))
      }
      override def apply(args: List[Value], env: Env): (Value, Env) = {
        val classSource: MemberDef = source(sym.owner)
        val (paramss, earlydefns, stats:List[Tree], parents) = classSource match {
          case q"$_ class $_[..$_] $_(...$paramss) extends { ..$earlydefns } with ..$parents { $_ => ..$stats }" =>
            (paramss, earlydefns, stats, parents)
        }
        // process early defs before superclass constructor invocation
        val (res, env1) = eval(earlydefns, env.pushFrame(capturedEnv))
        val env2 = env1.extend(parent, earlydefns.map(_.symbol).zip(res).toMap)
        // map constructor argss to object field symbols
        val valDefs = extractConsArgs(classSource, paramss)
        val src = source(sym).asInstanceOf[DefDef].rhs
        new ObjectCtorFun(valDefs, src, capturedEnv, stats, parent).apply(args, env2)
      }
    }
    class ObjectCtorFun(paramss: List[List[Symbol]], body: Tree, capturedEnv: Env, stats: List[Tree], parent: ObjectValue)
      extends FunctionValue(paramss, body, capturedEnv) {
      def initTrait(target: Symbol, env: Env): Result = {
        val traitSrc: MemberDef = source(target)
        val (stats:List[Tree], earlydefns) = traitSrc match {
          case q"$_ trait $name extends { ..$earlydefns } with ..$parents  { $_ => ..$stats }" => (stats, earlydefns)
        }
        val env1 = eval(earlydefns, env)._2
        val env2 = eval(stats, env1)._2
        (parent, env2)
      }
      def initTraits(env: Env): Result = {
        val traits = sym.typeSignature.baseClasses.filter(t=> t.asClass.isTrait && !t.isJava).reverse
        val env1 = traits.foldLeft(env)((newEnv, newTrait) => initTrait(newTrait, newEnv)._2)
        (parent, env1)
      }
      override def apply(args: List[Value], callSiteEnv: Env): (Value, Env) = {
        // FIXME: deduplicate this using FunctionValue(note the differences: argss->fields, body->body+stats, expr->parent)
        paramss match {
          case x :: Nil => // single parameter list - invoke
            val (_, env2) = eval(body, callSiteEnv.extend(parent, x.zip(args).toMap)) // body contains only super.ctor call
            val (_, env3) = initTraits(env2)  // trait initialization is not inlined in trees passed to us
            val (_, env4) = eval(stats, env3) // rest of ctor exprs goes into stats
            (parent, callSiteEnv.extendHeap(env4))
          case x :: xs => // multi parameter list - return curried function with N-1 parameter list
            val env2 = callSiteEnv.extend(parent, x.zip(args).toMap)
            (new ObjectCtorFun(xs, body, env2, stats, parent), callSiteEnv.extendHeap(env2))
          case Nil =>      // empty paramlist - invoke without extending env
            val (_, env2) = eval(body, callSiteEnv)
            val (_, env3) = initTraits(env2)
            val (_, env4) = eval(stats, env3)
            (parent, callSiteEnv.extendHeap(env4))
        }
      }
    }
    def selectMethod(member: Symbol, env: Env, static: Boolean): Result = {
      // for super calls we select member from a class symbol provided by member itself
      // otherwise lookup a member from stored class symbol
      val mem = if (static && !member.isAbstract)
        member
      else if (static && member.isAbstract) {
        // FIXME: an attempt on abstract override lookup, hack
        // tl;dr find first non-abstract method(matching signature) among linearized parents of saved ClassSymbol
        val symbols = sym.typeSignature.baseClasses.map(_.typeSignature.member(member.name).alternatives.find(alt => alt.typeSignature == member.typeSignature).getOrElse(null))
        val bases = symbols.collect({case m: MethodSymbol => m}).filter(m => !m.isAbstractOverride && !m.isAbstract)
        bases.head
      }
      else
        sym.typeSignature.member(member.name).alternatives.find(alt => alt.typeSignature == member.typeSignature).get
      env.heap.get(this) match {
        case Some(Object(fields)) => (fields.getOrElse(mem, MethodValue(mem.asMethod, env)), env)
        case Some(_: Primitive)   => IllegalState(this, "a primitive couldn't have fields")
        case _ => IllegalState(this)
      }
    }
    override def select(member: Symbol, env: Env, static: Boolean = false): Result = {
      // point all parent symbol references to this instance
      val env1 = sym.typeSignature.baseClasses.foldLeft(env)((tmpEnv, parent) => tmpEnv.extend(parent, this))
      env1.heap.get(this) match {
        case _ if member.isJava        => super.select(member, env, static)
        case _ if member.isConstructor => (new ObjectCtor(this, member.asMethod, env1.extend(sym, this)), env)
        case _ if member.isMethod      => selectMethod(member, env1, static)
        case Some(Object(fields))      =>
          fields.get(member) match {
            case Some(value)  => (value, env)
            case _            => IllegalState(member, "no such field")
          }
        case _                         => UninitializedObject(this)
      }
    }
    override def toString = s"ObjectValue#" + id
  }
  
  class UninitializedModuleValue(mod: ModuleSymbol) extends ModuleValue(mod) {
    override def select(member: Symbol, env: Env, static: Boolean = false): (Value, Env) = {
      val (res, env1) = init(env)
      res.select(member, env.extend(mod, res).extendHeap(env1))
    }
    def collectFields(parents: List[Symbol], env: Env): Result = {
      parents match {
        case x::xs =>
          // java classes have no obtainable source
          if (x.isJava) Value.reflect((), env)
          else {
            val (_, env1) = collectFields(xs, env)
            val body: List[Tree] = source(x) match {
              case cd: ClassDef   => cd.impl.body
              case md: ModuleDef  => md.impl.body
              case other          => IllegalState(other)
            }
            val (_, env2) = eval(body, env1)
            Value.reflect((), env2)
          }
        case _     => Value.reflect((), env)
      }
    }
    override def init(env: Env): (Value, Env) = {
      val v = new ModuleValue(mod)
      val (_, env1) = collectFields(mod.typeSignature.baseClasses.filter(it => !it.isModuleClass && !it.isJava && it != AnyClass),
        // point all parent symbol references to this instance and allocate object in heap
        mod.typeSignature.baseClasses.foldLeft(env)((tmpEnv, s) => tmpEnv.extend(s, v)).extend(mod,v).extend(v, new Object(ListMap())))
      val (_, env2) = eval(body, env1)
      (v, env.extendHeap(env2))
    }
    override def toString = s"UninitializedModuleValue#" + id
  }

  class ModuleValue(val mod: ModuleSymbol) extends ObjectValue(mod, mod.typeSignature) {
    override def init(env: Env): Result = (this, env)
    override def toString = s"ModuleValue#" + id
//    override def hashCode() = mod.hashCode()
//    override def equals(obj: scala.Any): Boolean = {
//      if (this == obj) return true
//      if (obj == null) return false
//      if (!obj.isInstanceOf[ModuleValue]) false
//      else obj.asInstanceOf[ModuleValue].mod == this.mod
//    }
  }

  object Value {
    def reflect[T](any: T, env: Env)(implicit ev: TypeTag[T]): Result = {
      // wrap a JVM value in an interpreter value
      // strictly speaking, env is unnecessary here, because this shouldn't be effectful
      // but I'm still threading it though here, because who knows
      val value = new JvmValue(ev.tpe)
      (value, env.extend(value, any))
    }
    def reflectAny(any: Any, env: Env): Result = {
      any match {
        case x: Byte                  => Value.reflect(x, env)
        case x: Short                 => Value.reflect(x, env)
        case x: Char                  => Value.reflect(x, env)
        case x: Int                   => Value.reflect(x, env)
        case x: Long                  => Value.reflect(x, env)
        case x: Float                 => Value.reflect(x, env)
        case x: Double                => Value.reflect(x, env)
        case x: Boolean               => Value.reflect(x, env)
        case x: Unit                  => Value.reflect(x, env)
        case x: String                => Value.reflect(x, env)
        case null                     => Value.reflect(null, env)
        case _                        => IllegalState(any, s"$any is not a primitive")
      }
    }
    def function(params: List[Tree], body: Tree, env: Env): Result = {
      // wrap a function in an interpreter value using the provided lexical environment
      // note how useful it is that Env is immutable!
      (FunctionValue(List(params.map(_.symbol)), body, env), env)
    }
    def instantiate(tpe: Type, env: Env): Result = {
      // instantiate a type (can't use ClassSymbol instead of Type, because we need to support polymorphic arrays)
      // not sure whether we need env, because we don't actually call the constructor here, but let's have it just in case
      createInstance(tpe, env)
    }
    def module(mod: ModuleSymbol, env: Env): Result = {
      // create an interpreter value that corresponds to the object represented by the symbol
      createModule(mod, env)
    }
    def method(meth: MethodSymbol, env: Env): Result = {
      (MethodValue(meth, env), env)
    }
    // allows creating trees from interpreter values and then extracting those values from those trees
    implicit def liftableValue: Liftable[Value] = Liftable { v => Ident(termNames.WILDCARD).updateAttachment(v) }
    implicit def unliftableValue: Unliftable[Value] = Unliftable { case t if t.attachments.contains[Value] => t.attachments.get[Value].get }
  }

  type Result = (Value, Env)
  type JvmResult = (Any, Env)
  type Results = (List[Value], Env)
}