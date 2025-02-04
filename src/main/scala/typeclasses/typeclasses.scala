object TypeClasses {

  /*
   *Small Library to serialize some data to a standard format (HTML)
   * */
  // V1: The OO way
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml = s"<div>$name ($age) <a href=$email/></div>"
  }

  val bob2Html = User("Bob", 43, "bob@mail.com").toHtml

  /*
   *
   *Drawbacks:
   - only available for the types we write
   - can only provide ONE implementation
   * */

  // v2 : Pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any): String = value match {
      case User(name, age, email) => s"<div>$name ($age) <a href=$email/></div>"
      case _ =>
        throw new IllegalArgumentException("data structure not supported")
    }

  }
  /*
     Drawbacks:
     - lost type safety
     - need to modify a single piece of code every time
     - still ONE implementation
   * */

  // V3 - Type class
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  // part 2 - type class instances for the supported types
  given userSerializer: HTMLSerializer[User] with {
    override def serialize(value: User): String = {
      val User(name, age, email) = value
      s"<div>$name ($age) <a href=$email/></div>"
    }
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
