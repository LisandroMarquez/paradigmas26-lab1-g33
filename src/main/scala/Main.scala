object Main {

  def main(args: Array[String]): Unit = {

    val subscriptions = FileIO.readSubscriptions()
    val posts = FileIO.get_posts()
    val validPosts = Filters.removeEmptyPosts(posts)

    println(Formatters.formatTitle("Informe de Suscripciones"))

    subscriptions match {
      case Some(subs) =>

        subs.foreach { sub =>
          val filter_posts = validPosts.filter(_._1 == sub._1)
          val score = FileIO.total_score(filter_posts)
          val frequent_words = Analytics.topWords(filter_posts, 10)
          val first_5_post = FileIO.first_posts(filter_posts)

          println(
            Formatters.formatSubscriptionReport(
              sub._1,
              score,
              frequent_words,
              first_5_post
            )
          )
        }

        println(Formatters.formatTitle("Fin del Informe"))

        menuSubscriptions(subs, validPosts)

      case None =>
        println("Error leyendo suscripciones")
    }
  }

  def menuSubscriptions(
    subs: List[FileIO.Subscription],
    posts: List[FileIO.Post]
  ): Unit = {

    val names = MenuLogic.subscriptionNames(subs)

    Interactive.showMenu("Suscripciones disponibles", names)

    val subIndex = Interactive.askOption(names.length)

    if (subIndex == 0) {
      println("Fin del programa.")
    } else {

      MenuLogic.choose(subs, subIndex) match {
        case Some(selectedSub) =>
          menuPosts(subs, posts, selectedSub)

        case None =>
          println("Suscripción inválida.")
          menuSubscriptions(subs, posts)
      }
    }
  }

  def menuPosts(
    subs: List[FileIO.Subscription],
    posts: List[FileIO.Post],
    selectedSub: FileIO.Subscription
  ): Unit = {

    val subPosts = MenuLogic.postsBySubscription(posts, selectedSub._1)

    if (subPosts.isEmpty) {
      println("No hay posts disponibles.")
      menuSubscriptions(subs, posts)

    } else {

      val titles = MenuLogic.postTitles(subPosts)

      Interactive.showMenu(s"Posts en ${selectedSub._1}", titles)

      val postIndex = Interactive.askOption(titles.length)

      if (postIndex == 0) {
        menuSubscriptions(subs, posts)

      } else {

        MenuLogic.choose(subPosts, postIndex) match {
          case Some(post) =>
            println(Formatters.formatPost(post))

            menuPosts(subs, posts, selectedSub)

          case None =>
            println("Post inválido.")
            menuPosts(subs, posts, selectedSub)
        }
      }
    }
  }
}