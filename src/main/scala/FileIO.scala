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

  //$ Define alias (name, url)
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
    json.children.map{
      sub =>
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

  //$ Defining alias (name, title, selftext, date)
  type Post = (String, String, String, String)

  //! Auxiliary object for canonizing the date
  object TextProcessing {
    def formatDateFromUTC(utc: Long): String =
      java.time.Instant.ofEpochSecond(utc).toString
  }

  //$ A function that downloads posts and extracts their important data
  def get_posts(): List[Post] = {
    //& Download the JSON
    def downloadAllFeeds(): List[(String, String)] = {
      readSubscriptions().map{
        case (name, url) =>
          (name, downloadFeed(url))
      } 
    }

    //+ First parse the feeds
    val parse_list = downloadAllFeeds().map{
      case (name, jsonString) =>
        (name, parse(jsonString))
    }  

    //% Then extract the data with the canonized date 
    parse_list.flatMap{
      case (name, json) =>
        (json \ "data" \ "children").children.map{
          post => 
            val title = (post \ "data" \ "title").extract[String]
            val selftext = (post \ "data" \ "selftext").extract[String]
            val created_utc = (post \ "data" \ "created_utc").extract[Double].toLong
            val date = TextProcessing.formatDateFromUTC(created_utc)
            (name, title, selftext, date)
        }
    }
  }

  // Function to run the post list and extract the suscription data (maybe is bad)
  def total_statistics(sub : Subscription): Int = {
    def name_score(): (String, Int) = {
      // Filter the posts of the suscription 
      sub.filter(post._1 == sub._1) 
    }
      // For later 
      post_list.foldLeft(0)((acum, _) => acum + 1) 
  }  
