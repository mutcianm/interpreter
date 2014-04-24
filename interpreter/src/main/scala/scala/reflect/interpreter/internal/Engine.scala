package scala.reflect.interpreter
package internal

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

abstract class Engine extends InterpreterRequires with Definitions with Errors with Emulators {
  import u._
  import definitions._
  import internal.decorators._

  def eval(tree: Tree): Any = {
    // can only interpret fully attributes trees
    // which is why we can't test the interpreter on the output of reify
    // TODO: in Palladium this will become irrelevant, because all trees
    // are going to have a `Tree.tpe` method that will actually typecheck stuff if necessary
    tree.foreach(sub => {
      if (sub.tpe == null) UnattributedAst(sub)
      if (sub.symbol == NoSymbol) UnattributedAst(sub)
    })
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
    case q"$qual.$_"                          => evalSelect(qual, tree.symbol, env)
//    case q"$qual.super[$_].$_"                => evalSelect(q"$qual.this", tree.symbol, env)
    case q"$_.this"                           => env.lookup(tree.symbol)
    case q"$expr.isInstanceOf[$tpt]()"        => evalTypeTest(expr, tpt.tpe, env)
    case q"$expr.asInstanceOf[$tpt]()"        => evalTypeCast(expr, tpt.tpe, env)
    case Apply(expr, args)                    => evalApply(expr, args, env) // the q"$expr[..$targs](...$argss)" quasiquote is too high-level for this
    case TypeApply(expr, targs)               => eval(expr, env)
    case q"$lhs = $rhs"                       => evalAssign(lhs, rhs, env)
    case q"return $expr"                      => ???
    case q"throw $expr"                       => ???
    case q"$expr: $_"                         => eval(expr, env)
    // case q"(..$exprs)"                     => never going to happen, because parser desugars these trees into applications
    case q"if ($cond) $then1 else $else1"     => evalIf(cond, then1, else1, env)
    case q"$scrut match { case ..$cases }"    => evalMatch(scrut, cases, env)
    case q"try $expr catch { case ..$cases } finally $finally1" => ???
    case q"(..$params) => $body"              => Value.function(params, body, env)
    // case q"{ case ..$cases }"              => never going to happen, because typer desugars these trees into anonymous class instantiations
    case q"while ($cond) $body"               => evalWhile(cond, body, env)
    case q"do $body while ($cond)"            => evalDoWhile(cond, body, env)
    case s: Super                             => eval(s.qual, env)
    case q"{ ..$stats }"                      => evalBlock(tree.children, env) // FIXME: qq bug workaround(lazy value wrapper functions skipped in stats)
    // case q"for (..$enums) $expr"           => never going to happen, because parser desugars these trees into applications
    // case q"for (..$enums) yield $expr"     => never going to happen, because parser desugars these trees into applications
    // case q"new { ..$early } with ..$parents { $self => ..$stats }" => never going to happen in general case, desugared into selects/applications of New
    case _: ValDef | _: DefDef                => evalLocal(tree, env) // can't skip these trees, because we need to enter them in scope when interpreting
    case _: ModuleDef                         => evalModuleDef(tree, env)
//    case _: ClassDef                          => evalClassDef(tree, env)
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
    def reflect(jvmValue: Any) = Value.reflect(jvmValue, env)
    tree match {
      case q"${x: Byte}"                  => reflect(x)
      case q"${x: Short}"                 => reflect(x)
      case q"${x: Char}"                  => reflect(x)
      case q"${x: Int}"                   => reflect(x)
      case q"${x: Long}"                  => reflect(x)
      case q"${x: Float}"                 => reflect(x)
      case q"${x: Double}"                => reflect(x)
      case q"${x: Boolean}"               => reflect(x)
      case q"${x: Unit}"                  => reflect(x)
      case q"${x: String}"                => reflect(x)
      case q"null"                        => reflect(null)
      case Literal(Constant(tpe: Type))   => RuntimeReflectionNotSupported(tree) // this tree shape stands for `classOf[$tpe]` after typechecking
      case Literal(Constant(sym: Symbol)) => UnrecognizedAst(tree) // this is a very obscure tree shape only used in annotations, and we don't eval those
      case _                              => UnrecognizedAst(tree)
    }
  }

  def evalLocal(tree: Tree, env: Env): Result = {
    val sym = tree.symbol
    def defaultValue(tpe: Type): Result = {
      val jvmValue = tpe match {
        case tpe if tpe == UnitTpe    => ()
        case tpe if tpe == BooleanTpe => false
        case tpe if tpe == FloatTpe   => 0.0f
        case tpe if tpe == DoubleTpe  => 0.0d
        case tpe if tpe == ByteTpe    => 0.toByte
        case tpe if tpe == ShortTpe   => 0.toShort
        case tpe if tpe == IntTpe     => 0
        case tpe if tpe == LongTpe    => 0L
        case tpe if tpe == CharTpe    => 0.toChar
        case _                        => null
      }
      Value.reflect(jvmValue, env)
    }
    val (vrepr, env1) = tree match {
      case ValDef(_, _, tpt, rhs) =>
        // note how we bind ValDef's name to the default value of the corresponding type when evaluating the rhs
        // this is how Scala does it, so don't say that this looks weird :)
        val (vdefault, envx) = defaultValue(tpt.tpe)
        val (res, env1) = eval(rhs, envx.extend(tree.symbol, vdefault))
        res.copy(env1)
      case tree: DefDef =>
        Value.method(tree.symbol.asMethod, env)
    }
    val env2 = env.extendHeap(env1).extend(tree.symbol, vrepr)
    (vrepr, env2)
  }

  def evalModuleDef(tree: Tree, env: Env): Result = {
    Value.module(tree.symbol.asModule, tree.children.head.asInstanceOf[Template].body, env)
  }

  def evalClassDef(tree: Tree, env: Env): Result = {
    Value.reflect((), env)
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
        Value.reflect((), env1.extend(vlhs, lhs.symbol, vrhs))
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
    val (vqual, env1) = eval(qual, env)
    val (value, env2) = qual match {
      case _:Super  => vqual.select(sym, env1, _super = true)
      case _        => vqual.select(sym, env1)
    }
    value match {
      case MethodValue(meth, _) if meth.paramLists.isEmpty => value.apply(Nil, env2)
      case _                    => (value, env2)
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
    vexpr.apply(vargs, env2)
  }

  def evalIf(cond: Tree, then1: Tree, else1: Tree, env: Env): Result = {
    // TODO: handle reify side-effects in branches
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
          checkCond(q"$vscrut.${Any_isInstanceOf}[$tpt]()")
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
          checkCond(q"$vscrut.${Any_isInstanceOf}[$tpt]()", onSuccess = cont)
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
        val (vguard, env2) = eval(guard.orElse(q"true"), env1)
        vguard.branch(env3 => eval(body, env3), env3 => loop(vscrut, rest, env3), env2)
      case _ =>
        val (exn, env1) = Value.instantiate(typeOf[MatchError], env)
        val (exn1, env2) = exn.apply(List(vscrut), env1)
        ??? // TODO: throw exn1 in env2
    }
    val (vscrut, env1) = eval(scrut, env)
    loop(vscrut, cases, env1)
  }

  // can't make these classes final because of SI-4440
  sealed trait Slot
  case class Primitive(value: Any) extends Slot
  case class Object(fields: Map[Symbol, Value]) extends Slot
  type Heap = ListMap[Value, Slot]

  type FrameStack = List[ListMap[Symbol, Value]]

  case class Env(stack: FrameStack, heap: Heap) {
    def lookup(sym: Symbol): Result = {
      // TODO: handle lazy val init
      // TODO: handle module init
      // TODO: evaluate nullary methods
      // all the stuff above might be effectful
      // therefore we return Result here, and not just Value
      sym match {
        case m: ModuleSymbol                     => stack.head(sym).init(this)
        case mc: ClassSymbol if mc.isModuleClass => stack.head(mc.module).init(this)
        case _                                   => (stack.head(sym), this)
      }
    }
    def extend(sym: Symbol, value: Value): Env = {
      stack.head.get(sym) match {
        case Some(v: ReifiableValue) => Env(stack, heap + (v -> heap(value)))
        case Some(v: Value)          => Env(stack, heap) // TODO: what do with unreifiable values
        case None           => Env((stack.head + (sym -> value)) :: stack.tail, heap)
      }
    }
    def extend(obj: Value, field: Symbol, value: Value): Env = {
      // TODO: extend heap with the new value for the given field of the given object
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

  class ObjectEnv(stack: FrameStack, heap: Heap, symbolMappings: Map[Symbol, Symbol]) extends Env(stack, heap) {
    override def lookup(sym: Symbol): (Value, Env) = {
      super.lookup(symbolMappings.getOrElse(sym, sym))
    }
  }

  @volatile private var _nextId = new java.util.concurrent.atomic.AtomicInteger()
  private def nextId() = _nextId.incrementAndGet()

  sealed trait Value {
    val id = nextId()
    def reify(env: Env): JvmResult = {
      // convert this interpreter value to a JVM value
      // return None if it refers to a not-yet-compiled class
      // note that it is probably possible to improve reify to work correctly in all cases
      // however this doesn't matter much for Project Palladium, so that's really low priority
      // TODO: throw an exception when trying to reify an object
      env.heap.get(this) match {
        case Some(Primitive(prim)) => (prim, env)
        case Some(_)               => UnreifiableResult(this)
        case None                  => IllegalState(this)
      }
    }
    def select(member: Symbol, env: Env, _super: Boolean = false): Result = {
      // note that we need env here, because selection might be effectful
      // also note that there's no need to evaluate empty-arglist methods here
      // if we have an empty-arglist application, then the result of select will be fed into apply(Nil)
      // TODO: needs to handle selections of field and method references
      // because e.g. foo.bar(1, 2) looks like Apply(Select(foo, bar), List(1, 2))
      // TODO: same todos as for Env.lookup
      // TODO: implement all Any methods such as hashCode/etc here
      ???
    }
    def apply(args: List[Value], env: Env): Result = {
      // TODO: needs to work well both with functions and method references
      // TODO: also has to handle partial applications
      // TODO: when inside a method application, we should have enclosing this'es bound to corresponding class symbols
      // TODO: in the current model, constructors have to return the object being constructed
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
      // TODO: can't use a symbol here, because this can be an array. use tpe.erasure if in doubt
      ???
    }
    def typeCast(tpe: Type, env: Env): Result = {
      // TODO: can't be a no-op, because we actually need to throw if the type is incompatible
      // TODO: also, we have to deal with all sort of conversions built into Scala: boxings, numeric stuff, nulls, etc
      ???
    }
    def copy(env: Env): Result = (this, env)
    def init(env: Env): Result = (this, env)
  }

  trait ReifiableValue

  class JvmValue() extends Value with ReifiableValue with MagicMethodEmulator {
    override def select(member: Symbol, env: Env, _super: Boolean = false): Result = {
      (selectCallable(this, member, env), env)
    }
    override def copy(env: Env) = {
      val v = new JvmValue()
      val (res, env1) = reify(env)
      (v, env1.extend(v, res))
    }
    override def toString = s"JvmValue#" + id
  }

  sealed trait CallableValue extends Value {
    override def reify(env: Env): (Any, Env) = {
      val (res, env1) = apply(List(), env)
      res.reify(env1)
    }
  }

  case class EmulatedCallableValue(f: (List[Value], Env) => Result) extends CallableValue {
    override def apply(args: List[Value], env: Env) = f(args, env)
  }

  case class FunctionValue(paramss: List[List[Symbol]], body: Tree, capturedEnv: Env) extends CallableValue {
    override def apply(args: List[Value], callSiteEnv: Env): Result = {
      val env1 = callSiteEnv.pushFrame(capturedEnv)
      paramss match {
        case x :: Nil => // single parameter list - invoke
          val env2 = x.zip(args).foldLeft(env1)((tmpEnv, p) => tmpEnv.extend(p._1, p._2))
          val (res, env3) = eval(body, env2)
          (res, callSiteEnv.extendHeap(env3))
        case x :: xs => // multi parameter list - return curried function with N-1 parameter list
          val env2 = x.zip(args).foldLeft(env1)((tmpEnv, p) => tmpEnv.extend(p._1, p._2))
          (FunctionValue(xs, body, env2), callSiteEnv.extendHeap(env2))
        case _ =>      // empty paramlist - invoke without extending env
          val (res, env3) = eval(body, env1)
          (res, callSiteEnv.extendHeap(env3))
      }
    }

    override def select(member: Symbol, env: Env, _super: Boolean = false): Result = {
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
    override def toString = s"MethodValue#" + id
  }

  // a placeholder for java methods/methods with unobtainable source
  case class DummyMethodValue(sym: MethodSymbol) extends CallableValue {
    override def apply(args: List[Value], env: Env): (Value, Env) = {
      Value.reflect((), env)
    }
    override def toString = s"DummyMethodValue#" + id
  }

  class ObjectValue(sym: Symbol, body: List[Tree]) extends Value {
    var initialized = false
    class ObjectCons(parent: ObjectValue, sym: MethodSymbol, capturedEnv: Env) extends MethodValue(sym, capturedEnv) {
      def extractConsArgs(tree: MemberDef, args: List[List[ValDef]]): List[List[Symbol]] = {
        // workaround constructor args symbol resolution bug
        // extract symbols from valdefs of class body by name
        // FIXME: there seems to be another bug, this time with names: an extra whitespace is appended to name in valdef symbol
        val valDefs = tree.children.head.asInstanceOf[Template].body.collect({case d: ValDef => d}).map(it=>(it.name.toString.trim, it)).toMap
        args.map(params => params.map(it => valDefs(it.name.toString).symbol))
      }
      override def apply(args: List[Value], env: Env): (Value, Env) = {
        val (paramss, earlydefns, stats:List[Tree], parents) = source(sym.owner) match {
          case q"$_ class $_[..$_] $_(...$paramss) extends { ..$earlydefns } with ..$parents { $_ => ..$stats }" =>
            (paramss, earlydefns, stats, parents)
        }
        // map constructor args to object field symbols
        val valDefs = extractConsArgs(source(sym.owner), paramss.asInstanceOf[List[List[ValDef]]])
        val src = source(sym).asInstanceOf[DefDef].rhs
        new ObjectConsFun(valDefs, src, capturedEnv, stats, parent).apply(args, env)
      }
    }
    class ObjectConsFun(paramss: List[List[Symbol]], body: Tree, capturedEnv: Env, stats: List[Tree], parent: ObjectValue)
      extends FunctionValue(paramss, body, capturedEnv) {
      override def apply(args: List[Value], callSiteEnv: Env): (Value, Env) = {
        // FIXME: deduplicate this using FunctionValue(note the differences: args->fields, body->body+stats, expr->parent)
        val env1 = callSiteEnv.pushFrame(capturedEnv)
        paramss match {
          case x :: Nil => // single parameter list - invoke
            val (_, env2) = eval(body, env1.extend(parent, x.zip(args).toMap))
            val (res1, env3) = eval(stats, env2)
            val env4 = env3.extend(parent, stats.map(_.symbol).zip(res1).toMap)
            (parent, callSiteEnv.extendHeap(env4))
          case x :: xs => // multi parameter list - return curried function with N-1 parameter list
            val env2 = env1.extend(parent, x.zip(args).toMap)
            (new ObjectConsFun(xs, body, env2, stats, parent), callSiteEnv.extendHeap(env2))
          case _ =>      // empty paramlist - invoke without extending env
            val (_, env2) = eval(body, env1)
            val (res1, env3) = eval(stats, env2)
            val env4 = env3.extend(parent, stats.map(_.symbol).zip(res1).toMap)
            (parent, callSiteEnv.extendHeap(env4))
        }
      }
    }
    def selectOverride(member: Symbol, env: Env, _super: Boolean): Result = {
      // for super calls we select member from a class symbol provided by member itself
      // otherwise lookup a member from stored class symbol
      val mem = if (_super)
        member.owner.typeSignature.member(member.name)
      else
        sym.typeSignature.member(member.name)
      env.heap.get(this) match {
        case Some(Object(fields)) => (fields.getOrElse(mem, MethodValue(mem.asMethod, env)), env)
        case Some(_: Primitive)   => IllegalState(this, "an object couldn't be a primitive")
        case _ => IllegalState(this)
      }
    }
    override def select(member: Symbol, env: Env, _super: Boolean = false): Result = {
      // point all parent symbol references to this instance
      val env1 = sym.typeSignature.baseClasses.foldLeft(env)((tmpEnv, parent) => tmpEnv.extend(parent, this))
      env.heap.get(this) match {
        // FIXME: we should emulate java.Object methods somehow
        case _ if member.isJava   => (DummyMethodValue(member.asMethod), env)
        case _ if member.isConstructor =>
          (new ObjectCons(this, member.asMethod, env1.extend(sym, this)), env)
        case _ if member.isMethod => selectOverride(member, env1, _super)
        case Some(Object(fields)) =>
          fields.get(member) match {
            case Some(value)             => (value, env)
            case _                       => IllegalState(member, "no such field")
          }
        case _                    => UninitializedObject(this)
      }
    }
    override def toString = s"ObjectValue#" + id
  }

  case class ModuleValue(mod: ModuleSymbol, body: List[Tree]) extends ObjectValue(mod, body) {
    override def select(member: Symbol, env: Env, _super: Boolean = false): (Value, Env) = {
      val (_, env1) = init(env)
      super.select(member, env.extendHeap(env1))
    }
    def collectFields(parents: List[Symbol], env: Env): (Map[Symbol, Value], Env) = {
      parents match {
        case x::xs =>
          // java classes have no obtainable source
          if (x.isJava) (Map(), env)
          else {
            // assuming result of baseClasses is correctly linearized
            val (res1, env1) = collectFields(xs, env)
            val body: List[Tree] = source(x) match {
              case cd: ClassDef   => cd.children.head.asInstanceOf[Template].body
              case md: ModuleDef  => md.children.head.asInstanceOf[Template].body
              case other          => IllegalState(other)
            }
            val (res2: List[Value], env2) = eval(body, env1)
            (body.map(_.symbol).zip(res2).toMap ++ res1, env2)
          }
        case _     => (Map(), env)
      }
    }
    override def init(env: Env): (Value, Env) = {
      if (initialized) {
        (this, env)
      } else {
        initialized = true
        val (inherited, env1) = collectFields(mod.typeSignature.baseClasses.filter(!_.isModuleClass),
          mod.typeSignature.baseClasses.foldLeft(env)((tmpEnv, s) => tmpEnv.extend(s, this)))
        val (res: List[Value], env2) = eval(body, env1.extend(mod, this))
        val env3 = env.extendHeap(env2).extend(this, Object(inherited ++ body.map(_.symbol).zip(res).toMap))
        (this, env3)
      }
    }
    override def toString = s"ModuleValue#" + id
  }

  object Value {
    def reflect(any: Any, env: Env): Result = {
      // wrap a JVM value in an interpreter value
      // strictly speaking, env is unnecessary here, because this shouldn't be effectful
      // but I'm still threading it though here, because who knows
      val value = new JvmValue()
      (value, env.extend(value, any))
    }
    def function(params: List[Tree], body: Tree, env: Env): Result = {
      // wrap a function in an interpreter value using the provided lexical environment
      // note how useful it is that Env is immutable!
      (FunctionValue(List(params.map(_.symbol)), body, env), env)
    }
    def instantiate(tpe: Type, env: Env): Result = {
      // TODO: instantiate a type (can't use ClassSymbol instead of Type, because we need to support polymorphic arrays)
      // not sure whether we need env, because we don't actually call the constructor here, but let's have it just in case
      val v = new ObjectValue(tpe.typeSymbol, source(tpe.typeSymbol).children.head.asInstanceOf[Template].body)
      (v, env.extend(tpe.typeSymbol, v).extend(v, new Object(Map())))
    }
    def module(mod: ModuleSymbol, children: List[Tree], env: Env): Result = {
      // TODO: create an interpreter value that corresponds to the object represented by the symbol
      val value = ModuleValue(mod, children)
      (value, env.extend(mod, value))
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