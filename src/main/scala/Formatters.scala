object Formatters {

  // Pure function to format posts from a subscription
  def formatSubscription(url: String, posts: String): String = {
    val header = s"\n${"=" * 80}\nPosts from: $url \n${"=" * 80}"
    val formattedPosts = posts.take(80)
    header + "\n" + formattedPosts
  }

  // Pure function to format a subscription report
  def formatSubscriptionReport(
    name: String,
    score: Int,
    words: List[(String, Int)],
    posts: List[(String, String, String)]
  ): String = {

s"""
+--------------------------------------------------------------------------------------------------+
Nombre: $name
Score total: $score
Top 10 Palabras con mayor frecuencia: $words
Primeros 5 post (Titulo, fecha, url): $posts
"""
  }

// Pure function to format post details
def formatPost(post: FileIO.Post): String = {

s"""
+--------------------------------------------------------------------------------------------------+
Titulo: ${post._2}
Fecha: ${post._4}
Score: ${post._5}
URL: ${post._6}

Contenido:
${post._3}
+--------------------------------------------------------------------------------------------------+
"""
  }

  // Pure function to display a title with borders
  def formatTitle(title: String): String = {
    "\n" +
      "+" + "=" * 98 + "+\n" +
      f"| ${title}%-96s |\n" +
      "+" + "=" * 98 + "+"
  }
}