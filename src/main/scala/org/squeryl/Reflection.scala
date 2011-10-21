package org.squeryl

object Reflection {

  implicit def inspection[T <: Any](obj: T) = new {
    def respondsTo(id: Symbol) =
      obj.asInstanceOf[AnyRef].getClass.getMethods.find(_.getName == id.name).isDefined
    
    def send(id: Symbol, args: AnyRef*) = {
      val method = obj.asInstanceOf[AnyRef].getClass.getMethods.find { m =>
        m.getName == id.name && m.getParameterTypes.length == args.size
      }
      method.map(_.invoke(obj, args: _*))
    }
  }
}
