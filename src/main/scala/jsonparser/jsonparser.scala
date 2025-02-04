import java.util.Date
import scala.compiletime.ops.string
import JSONSerialization.JSONConverter.convert
object JSONSerialization {
  /*

Users, posts, feeds
Serialize to JSON
   */

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createAt: Date)
  case class Feed(user: User, posts: List[Post])

  /*
   * Intemediate data: numbers, strings, lists, dates, objects
   * type class to convert data to intermediate data
   * serialize to JSON
   */

  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }
  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String =
      values.map(_.stringify).mkString("[", ",", "]")
  }
  final case class JSONObject(values: Map[String, JSONValue])
      extends JSONValue {
    override def stringify: String = values
      .map { case (key, value) =>
        "\"" + key + "\":" + value.stringify
      }
      .mkString("{", ",", "}")
  }

  // part 2 - type class pattern
  // 1 - TC Definition

  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }
  // 2 - TC instances (String, int, Date, User, Post, Feed)

  given stringConverter: JSONConverter[String] with
    override def convert(value: String): JSONValue = JSONString(value)

  given intConverter: JSONConverter[Int] with
    override def convert(value: Int): JSONValue = JSONNumber(value)

  given dateConverter: JSONConverter[Date] with
    override def convert(value: Date): JSONValue = JSONString(value.toString)

  given userConverter: JSONConverter[User] with
    override def convert(user: User) = JSONObject(
      Map(
        "name" -> JSONConverter.convert(user.name),
        "age" -> JSONConverter.convert(user.age),
        "email" -> JSONConverter.convert(user.email)
      )
    )

    given postConverter: JSONConverter[Post] with
      override def convert(value: Post): JSONValue = JSONObject(
        Map(
          "content" -> JSONConverter.convert(value.content),
          "createAt" -> JSONConverter.convert(value.createAt.toString)
        )
      )
    given feedConverter: JSONConverter[Feed] with
      override def convert(feed: Feed): JSONValue = JSONObject(
        Map(
          "user" -> JSONConverter.convert(feed.user),
          "posts" -> JSONArray(
            feed.posts.map(post => JSONConverter.convert(post))
          )
        )
      )

  // 3 - user-facing API
  object JSONConverter {
    def convert[T](value: T)(using converter: JSONConverter[T]): JSONValue =
      converter.convert(value)
    def apply[T](using instance: JSONConverter[T]): JSONConverter[T] = instance
  }
  // 4 - extension methods
  //
  object JSONSyntax {
    extension [T](value: T)
      def toJSON(using converter: JSONConverter[T]): JSONValue =
        converter.convert(value)
  }
  val data = JSONObject(
    Map(
      "user" -> JSONString("Breno"),
      "posts" -> JSONArray(List(JSONString("Hello"), JSONNumber(42)))
    )
  )

  val user = User("Breno", 25, "breno@mail.com")
  def main(args: Array[String]): Unit = {
    println(data.stringify)
    import JSONSyntax.*
    println(user.toJSON.stringify)
  }
}
