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
    
    //println(validPosts)

    //+ Exercise 4


    //+ Exercise 5


    //+ Exercise 6
    println(s""" 
=================================================================================================================================================
                                                           NFORME SOBRE SUSCRIPCIONES
-------------------------------------------------------------------------------------------------------------------------------------------------
""")
    subscriptions match {
      case Some(subs) =>
        subs.foreach { sub =>
        val filter_posts = posts.filter(_._1 == sub._1)
        val score = FileIO.total_score(filter_posts)
        val frecuent_words = Analytics.topWords(filter_posts, 10) 
        val first_5_post = FileIO.first_posts(filter_posts) 
        println(s"""
==================================================================== NOMBRE ====================================================================
                                                                   ${sub._1}
------------------------------------------------------------------------------------------------------------------------------------------------
                                                                  Score total: 
                                                                    $score
------------------------------------------------------------------------------------------------------------------------------------------------
                                                        Top 10 Palabras con mayor frecuencia: 
     $frecuent_words
------------------------------------------------------------------------------------------------------------------------------------------------
                                                       Primeros 5 post (Titulo, fecha, url):
$first_5_post 
================================================================================================================================================
 """)
      }

      case None =>
      println("Error leyendo suscripciones") 
    } 
  }
}