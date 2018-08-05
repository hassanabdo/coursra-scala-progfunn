package objsets

import TweetReader._

/**
  * A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

object Tweet{
  var tmp:Tweet = new Tweet("","",Integer.MIN_VALUE)
}

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
  * larger.
  *
  * Note that the above structure requires us to be able to compare two tweets (we
  * need to be able to say which of two tweets is larger, or if they are equal). In
  * this implementation, the equality / order of tweets is based on the tweet's text
  * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
  * text from different users.
  *
  *
  * The advantage of representing sets as binary search trees is that the elements
  * of the set can be found quickly. If you want to learn more you can take a look
  * at the Wikipedia page [1], but this is not necessary in order to solve this
  * assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet {

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
    * This is a helper method for `filter` that propagates the accumulated tweets.
    */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def mostRetweeted: Tweet

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def descendingByRetweet: TweetList = {
    var list: TweetList = Nil

    def descendingByRetweetUtil(tweetSet: TweetSet): Unit = {
      var tweet: Tweet = new Tweet("", "", -1)
//      println(tweet)
      try {
        val tweet2 = tweetSet.mostRetweeted
        tweet = new Tweet(tweet2.user, tweet2.text, tweet2.retweets)
        Tweet.tmp = new Tweet("","",Integer.MIN_VALUE)
      } catch {
        case e: NoSuchElementException => return
      }
//      println("--------------------------------------------------------")
//      println(tweet)
//      println("descendingByRetweetUtil        "+tweetSet.contains(tweet))
      descendingByRetweetUtil( tweetSet remove tweet )
//      println("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
      list = new Cons(tweet, list)
    }

//    println("inside descending ")
    descendingByRetweetUtil(this)
    list
  }

  /**
    * The following methods are already implemented
    */

  /**
    * Returns a new `TweetSet` which contains all elements of this set, and the
    * the new element `tweet` in case it does not already exist in this set.
    *
    * If `this.contains(tweet)`, the current set is returned.
    */
  def incl(tweet: Tweet): TweetSet

  /**
    * Returns a new `TweetSet` which excludes `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /**
    * Tests if `tweet` exists in this `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /**
    * This method takes a function and applies it to every element in the set.
    */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  /**
    * The following methods are already implemented
    */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {

    var newTweetSet: TweetSet = acc
    if (p(this.elem)) {
      //      println("sssssssssss"+ this.elem)
      newTweetSet = acc incl (this.elem)
    }
    val leftReduction = this.left.filterAcc(p, newTweetSet)
    this.right.filterAcc(p, leftReduction)
  }

  def mostRetweeted: Tweet = {
//    println("most right now " + Tweet.tmp)
    if (this.elem.retweets > Tweet.tmp.retweets) {
      Tweet.tmp = new Tweet(this.elem.user, this.elem.text, this.elem.retweets)
    }
//    println("peeeeeeeeeeeeeeep")
    try {
      this.left.mostRetweeted
    } catch {
      case e: Exception => Tweet.tmp
    }
    try {
      this.right.mostRetweeted
    } catch {
      case e: Exception => Tweet.tmp
    }
    Tweet.tmp
  }


  def union(that: TweetSet): TweetSet = {
    var newSet = that
    if (!that.contains(this.elem)) {
      newSet = that.incl(this.elem)
    }
    this.right.union(this.left.union(newSet))
  }


  /**
    * The following methods are already implemented
    */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet

  def tail: TweetList

  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def getTweets(list: List[String]): TweetSet = allTweets.filter(tweet => list.exists(str => tweet.text.contains(str)))

  lazy val googleTweets: TweetSet = getTweets(google)
  lazy val appleTweets: TweetSet = getTweets(apple)


  def merge(list1: TweetList, list2: TweetList): TweetList = {
    if(list1.isEmpty) return list2
    if(list2.isEmpty) return list1
    if (list1.head.retweets > list2.head.retweets) {
      new Cons(list1.head, merge(list1.tail, list2))
    } else {
      new Cons(list2.head, merge(list1, list2.tail))
    }
  }

  /**
    * A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */
  lazy val trending: TweetList = merge(googleTweets.descendingByRetweet, appleTweets.descendingByRetweet)
}

object Main extends App {
  //  Print the trending tweets
    GoogleVsApple.trending foreach println
  //  println("Heeeeeeeeeeeeeey Objsets")
  //  val tweets = allTweets
  //  println("peeeeeeeeeep")
  //  tweets foreach println


  //  allTweets foreach println
  //  println("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
  //  allTweets.filter(tweet => tweet.text.contains("Lightning")) foreach println
  //  println("yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy")


//    GoogleVsApple.googleTweets foreach println
//  println("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
//  println(GoogleVsApple.googleTweets.mostRetweeted)
//  println(GoogleVsApple.appleTweets.mostRetweeted)

//  GoogleVsApple.googleTweets.descendingByRetweet foreach println


//  val tweet = GoogleVsApple.googleTweets.mostRetweeted
//  GoogleVsApple.googleTweets foreach println
//  GoogleVsApple.googleTweets.remove(tweet) foreach println
}
