object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    //+ Exercise 1
    val subscriptions = FileIO.readSubscriptions()

    println(subscriptions)

    //+ Exercise 2
    val posts = FileIO.get_posts() 

    // println(posts) 

    //+ Exercise 3
    val validPosts = Filters.removeEmptyPosts(posts)
    
    println(validPosts)

    //+ Exercise 4


    //+ Exercise 5


    //+ Exercise 6
    println("PRUEBA DEL SCORE Y NAME DEL EJERCICIO 6")
    subscriptions match {
      case Some(subs) =>
        subs.foreach { sub =>
        val filter_posts = posts.filter(_._1 == sub._1)
        val score = FileIO.total_score(filter_posts)
        println(s"Name: ${sub._1} Score: $score")
      }

      case None =>
      println("Error leyendo suscripciones")
    } 

  } 
}