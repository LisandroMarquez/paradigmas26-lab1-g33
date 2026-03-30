import FileIO.Post

object Filters {
  //$ Individual filters for each case
  def formatEmptySelftext(posts: List[Post]): List[Post] = {
    posts.filter {
      case (_, _, selftext, _, _)=>
        selftext.trim.nonEmpty
    }
  }

  def formatEmptyTitle(posts: List[Post]): List[Post] = {
    posts.filter {
      case (_, title, _, _, _)=>
        title.trim.nonEmpty
    }
  }
  /* Explanation for the future
  *  [].trim removes every space, so we do not need an extra function to filter posts with only empty spaces
  *  [].trim.nonEmpty returns a boolean:
  *    true if contains text that is not only " " or ""
  *    false if does not contains anything but " " or ""
  */

  //$ Function to filter post completely
  def removeEmptyPosts(posts: List[Post]): List[Post] = {
    val postsWithoutEmptySelftext = formatEmptySelftext(posts)
    formatEmptyTitle(postsWithoutEmptySelftext)
  }
}