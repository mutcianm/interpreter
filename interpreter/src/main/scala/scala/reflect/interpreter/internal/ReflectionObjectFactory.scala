package scala.reflect.interpreter
package internal
import scala.reflect.runtime.{universe => ru}

trait ReflectionObjectFactory {
  self: Engine =>
  import u._
  import definitions._

  val rm = ru.runtimeMirror(getClass.getClassLoader)

  class ReflectedObjectValue(val javaClass: Class[_], sym: Symbol, tpe: Type) extends TypedValue(tpe) {
    var instance: Any = null
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.isConstructor) ???
      else if (member.isMethod) {
        (new ReflectedMethod(member, instance, javaClass), env)
      }
      else ??? // TODO: field selection goes here
    }

    override def reify(env: Env): (Any, Env) = (instance, env)
  }

  class WrappedJavaValue(val inst: AnyRef, override val javaClass: Class[_]) extends ReflectedObjectValue(javaClass, NoSymbol, NoSymbol.typeSignature) {
    instance = inst
  }

  object TypeConverter {
    def scalaToJava(in: Symbol): Class[_] = in match {
      case _ if in == AnyClass => classOf[java.lang.Object]
      case other    => rm.runtimeClass(in.asClass.asInstanceOf[ru.ClassSymbol])
    }
  }

  class ReflectedMethod(val method: Symbol, parentInstance: Any, parentClass: Class[_]) extends CallableValue {
    val reflectedMethod = if(parentInstance!=null) {
      val types = method.typeSignature.paramLists.head.map(it=>TypeConverter.scalaToJava(it.typeSignature.typeSymbol))
      parentInstance.getClass.getMethod(method.name.decodedName.toString, types:_*)
    } else {
      val types: List[Class[_]] = method.typeSignature.paramLists.head.map(_.typeSignature.typeSymbol.asClass.asInstanceOf[Class[_]])
      parentClass.getMethod(method.name.decodedName.toString, types: _*)
    }
    override def apply(args: List[Value], env: Env): (Value, Env) = {
      // return ReflectedObjectValue if call returns an object
      // otherwise return reflected primitive
      reflectedMethod.setAccessible(true)
      val res = reflectedMethod.invoke(parentInstance, args.map(_.reify(env)._1.asInstanceOf[java.lang.Object]):_*)
      res match {
        case x: java.lang.Byte => Value.reflect(x.toByte, env)
        case x: java.lang.Short => Value.reflect(x.toShort, env)
        case x: java.lang.Integer => Value.reflect(x.toInt, env)
        case x: java.lang.Long => Value.reflect(x.toLong, env)
        case x: java.lang.Float => Value.reflect(x.toFloat, env)
        case x: java.lang.Double => Value.reflect(x.toDouble, env)
        case x: java.lang.Boolean => Value.reflect(x.booleanValue(), env)
        case x: java.lang.String => Value.reflect(x, env)
        case null => Value.reflect(null, env)
        case _    => (new WrappedJavaValue(res, res.getClass), env)
      }
    }
  }

  class ReflectedField extends Value {

  }

  class ReflectedModuleValue(sym: ModuleSymbol) extends ReflectedObjectValue(java.lang.Class.forName(sym.fullName), sym, sym.typeSignature) {
  }

  class ReflectedScalaMethod(val method: MethodSymbol, parentValue: ReflectedScalaObject) extends CallableValue {
    override def apply(args: List[Value], env: Env): (Value, Env) = {
      val rr = ru.runtimeMirror(getClass.getClassLoader)
      val valueMirror = rr.reflect(parentValue.instance)
      val methodMirror = valueMirror.reflectMethod(method.asInstanceOf[ru.MethodSymbol])
      val args1 = args.map(_.reify(env)._1)
      val res = methodMirror.apply(args1:_*)
      res match {
        case x: java.lang.Byte => Value.reflect(x.toByte, env)
        case x: java.lang.Short => Value.reflect(x.toShort, env)
        case x: java.lang.Integer => Value.reflect(x.toInt, env)
        case x: java.lang.Long => Value.reflect(x.toLong, env)
        case x: java.lang.Float => Value.reflect(x.toFloat, env)
        case x: java.lang.Double => Value.reflect(x.toDouble, env)
        case x: java.lang.Boolean => Value.reflect(x.booleanValue(), env)
        case x: java.lang.String => Value.reflect(x, env)
        case null => Value.reflect(null, env)
        case _    => (new WrappedScalaValue(res, rm.reflect(res).symbol.typeSignature.asInstanceOf[Type]) , env)
      }
      
    }
  }

  class ReflectedScalaCtor(val method: MethodSymbol, parentValue: ReflectedScalaObject) extends CallableValue {
    override def apply(args: List[Value], env: Env): (Value, Env) = {
      val ctor = parentValue.cm.reflectConstructor(method.asInstanceOf[ru.MethodSymbol])
      parentValue.instance = ctor(args.map(_.reify(env)._1):_*)
      (parentValue, env)
    }
  }

  class ReflectedScalaObject(tpe: Type) extends TypedValue(tpe) {
    var instance: Any = null
    val rr = ru.runtimeMirror(getClass.getClassLoader)
    lazy val cm = rr.reflectClass(tpe.typeSymbol.asInstanceOf[ru.ClassSymbol])
    override def select(member: Symbol, env: Env, static: Boolean): (Value, Env) = {
      if (member.isConstructor) (new ReflectedScalaCtor(member.asMethod, this), env)
      else if (member.isMethod) (new ReflectedScalaMethod(member.asMethod, this), env)
      else ??? // TODO: fields
    }
  }

  class WrappedScalaValue(inst: Any, tpe: Type) extends ReflectedScalaObject(tpe) { instance = inst }

  def reflectInstance(tpe: Type, env: Env): Result = {
    (new ReflectedScalaObject(tpe), env)
  }

  def reflectModule(mod: ModuleSymbol, env: Env): Result = {
    (new ReflectedModuleValue(mod), env)
  }
}

