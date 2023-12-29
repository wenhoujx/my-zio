trait Animal
case object Cat extends Animal
case object Dog extends Animal

case class Box[+A](value: A):
  def set[A1 >: A](newValue: A1) = Box(newValue)

object Main extends App:
  program

  lazy val program =
    val catBox = Box(Cat)
    val animalBox: Box[Animal] = catBox
    println(catBox.set(Dog))
    val dogBox = animalBox.set(Dog)
    println(dogBox)
