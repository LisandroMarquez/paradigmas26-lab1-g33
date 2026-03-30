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
  def readSubscriptions(): Option[List[Subscription]] = {

    try {
      //& Read JSON File
      val source = Source.fromFile("subscriptions.json")

      //? Convert to String and close fd
      val jsonString = try source.mkString finally source.close()

      //+ Convert String to "JSON" format
      val json = parse(jsonString)

      //! Now we convert to List[Subscription] format
      val subscriptions = json.children.flatMap { sub =>
        for { 
          name <- (sub \ "name").extractOpt[String]
          url <- (sub \ "url").extractOpt[String]
        } yield (name, url)
      }

      Some(subscriptions)
    } catch {
      case e: Exception => None
    }
  }

  //$ Pure function to download JSON feed from a URL
  def downloadFeed(url: String): Option[String] = {
    try {
      val source = Source.fromURL(url)
      //& We need to close the source after reading, so we use a try-finally block
      val jsonString = try source.mkString finally source.close()
      Some(jsonString) 
    } catch {
      case _: Exception => None
    }
  }

  //$ Defining alias (name, title, selftext, date)
  // Int is a modification of Exercise 6 
  type Post = (String, String, String, String, Int)

  //! Auxiliary object for canonizing the date
  object TextProcessing {
    def formatDateFromUTC(utc: Long): String =
      java.time.Instant.ofEpochSecond(utc).toString
  }

  //$ A function that downloads posts and extracts their important data
  def get_posts(): List[Post] = {
    //& Download the JSON
    def downloadAllFeeds(): List[(String, String)] = {
      readSubscriptions()
      .getOrElse(List.empty)
      .flatMap{ case (name, url) =>
          downloadFeed(url).map(json => (name, json))
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
        (json \ "data" \ "children").children.flatMap{
          post => 
          for {
            title <- (post \ "data" \ "title").extractOpt[String]
            selftext <- (post \ "data" \ "selftext").extractOpt[String]
            created_utc <- (post \ "data" \ "created_utc").extractOpt[Double].map(_.toLong)
            date = TextProcessing.formatDateFromUTC(created_utc)
            // Modification of exercise 6 
            score <- (post \ "data" \ "score").extractOpt[Int]
          } yield (name, title, selftext, date, score)
        }
    }
  }

  // Function to run the post list and extract the suscription data (maybe is bad)
  def total_score(posts: List[Post]): Int = {
    // Calculate the sub score
    posts.foldLeft(0)((acum, posts) => acum + posts._5) 
  }  

  // Function to extract the first five posts
  //def first_posts(subs_posts: List[Post]): List[Post] = {
    
  //}
}