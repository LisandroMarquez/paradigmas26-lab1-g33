import FileIO.Post

object Filters {
  // Function to remove posts with empty title or selftext
  def removeEmptyPosts(posts: List[Post]): List[Post] = {
    posts.filter {
      case (_, title, selftext, _) =>
        title.trim.nonEmpty && selftext.trim.nonEmpty
    }
  }
}