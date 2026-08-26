package io.sphere.json

object AnotherFileExample {
  sealed abstract class Bug {
    def name: String
    def legs: Int
  }
  abstract class Insect extends Bug {
    def legs = 6
  }
  case class Spider(name: String) extends Bug {
    def legs = 8
  }

  case class Ant(name: String) extends Insect
  case class Grasshopper(name: String) extends Insect
}
