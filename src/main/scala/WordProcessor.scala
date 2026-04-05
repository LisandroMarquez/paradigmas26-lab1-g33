import scala.io.Source
import FileIO.Post

object Analytics {
  //$ We define a set of stopwords to filter out common words that do not carry significant meaning
  val stopwords = Set(
    "the", "about", "above", "after", "again", "against", "all", "am", "an",
    "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
    "before", "being", "below", "between", "both", "but", "by", "can't",
    "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
    "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
    "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
    "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
    "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
    "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
    "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
    "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
    "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
    "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
    "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
    "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
    "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
    "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
    "what's", "when", "when's", "where", "where's", "which", "while", "who",
    "who's", "whom", "why", "why's", "with", "won't", "would",
    "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours",
    "yourself", "yourselves"
  )

  //$ Function to count word frequencies in the selftext of the posts
  def countWordFrequencies(posts: List[Post]): Map[String, Int] = {
    posts
      //% Convert list of posts into a list of words by splitting the selftext of each post into words, \\s+ is a regex that matches one or more whitespace characters
      .flatMap (
        p => p._3.split("\\s+")
      )
      //% Filter out empty words, "stopwords", or do not start with an uppercase letter
      .filter(
        word =>
          word.nonEmpty
          && !stopwords.contains(word.toLowerCase)
          && word(0).isUpper
       )
      //% Group the words by their identity, which means that we create a map where the keys are the unique words and the values are lists of occurrences of those words
      .groupBy(Word => Word)
      //% Calculate the frequency of each word by counting the number of occurrences in each list
      .mapValues(_.size)
      .toMap
  }

  //$ Function to get the top n most frequent words
  def topWords(posts: List[Post], n: Int): List[(String, Int)] = {
    countWordFrequencies(posts)
      .toList
      //% Sort by frequency in descending order
      .sortBy(-_._2)
      //% Consider only top n words
      .take(n)
  }
}