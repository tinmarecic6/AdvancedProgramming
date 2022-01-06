sealed trait Binary
case object Empty extends Binary
case class O (t:Binary) extends Binary
case class I (t:Binary) extends Binary