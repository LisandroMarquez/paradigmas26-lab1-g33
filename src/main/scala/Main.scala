object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    //+ Exercise 1
    val subscriptions = FileIO.readSubscriptions()

    println(subscriptions)

    //+ Exercise 2
    val posts = FileIO.get_posts() 

    println(posts) 
    //+ Exercise 3


    //+ Exercise 4


    //+ Exercise 5


    //+ Exercise 6
    
  }
}
