object MenuLogic {

  // Helper function to create a numbered list of items
  def numberedList[A](items: List[A]): List[(Int, A)] =
    items.zipWithIndex.map {
      case (item, i) => (i + 1, item)
    }

  // Function to safely get an item from a list by index 
  def choose[A](items: List[A], index: Int): Option[A] =
    items.lift(index - 1)

  // Function to get the names of subscriptions
  def subscriptionNames(subs: List[FileIO.Subscription]): List[String] =
    subs.map(_._1)

  // Function to filter posts by subscription name
  def postsBySubscription(posts: List[FileIO.Post], subName: String): List[FileIO.Post] =
    posts.filter(_._1 == subName)

  // Function to get the titles of posts
  def postTitles(posts: List[FileIO.Post]): List[String] =
    posts.map(_._2)
}