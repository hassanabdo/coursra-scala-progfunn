package funsets

object Main extends App {
  import FunSets._
  val set1 = singletonSet(1)
  val set2 = singletonSet(2)
  println("test union : " + contains(union(set1, set2), 10))
  printSet(union(set1, set2))
  println("test intersect : " + contains(intersect(set1, set2), 1))
  printSet(intersect(set1, set2))

  val set3 = singletonSet(3)
  val set4 = singletonSet(4)
  val set5 = singletonSet(5)
  printSet(filter(union(set3, union(set4, set5)), x => x<5))

  println(forall(union(set3, union(set4, set5)), x => x<6))
  println(exists(union(set3, union(set4, set5)), x => x==4))

  printSet(map(union(set3, union(set4, set5)), x=>x-10))
}
