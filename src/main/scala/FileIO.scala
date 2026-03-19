import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {
  /*
    I asked ChatGPT why it is necessary, apparently json4s does not know how to convert JSON to scala types.
    I do not even know why would you create a JSON library and not make to convert types in any way possible,
      via function or something. This is unbealivable...
  */
  implicit val formats: DefaultFormats.type = DefaultFormats

  //$ Define alias
  type Subscription = (String, String)

  //$ Pure function to read subscriptions from a JSON file
  def readSubscriptions(): List[Subscription] = {
    //& Read JSON File
    val source = Source.fromFile("subscriptions.json")

    //? Convert to String and close fd
    val jsonString =
      try source.mkString
      finally source.close()

    //+ Convert String to "JSON" format
    val json = parse(jsonString)

    //! Now we convert to List[Subscription] format
    json.children.map { sub =>
      val name = (sub \ "name").extract[String]
      val url = (sub \ "url").extract[String]
      (name, url)
    }
  }

  //$ Pure function to download JSON feed from a URL
  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    source.mkString
  }
}
