class Container[+A](val elems: List[A]) {
  def get(i: Int): A = elems(i)

  def put[B >: A](elem: B) = new Container(elem :: elems)
}

class Animal(val name: String, val kind: String)

class Cat(name: String) extends Animal(name, "Cat")

class Dog(name: String) extends Animal(name, "Dog")

val anim1: Animal = new Animal("Booboo", "Baboon")
val cat1 = new Cat("Miaou")
// Standard polymorphism
val anim2: Animal = cat1

val dog1: Dog = new Dog("Choucroute")
val anim3: Animal = dog1

// Making an animal collection
val animalCollection = new Container[Animal](Nil).put(anim1)

// Making a cat container
val catCollection = new Container[Cat](Nil).put(cat1).put(new Cat("Garfield"))

// Polymorphism applied to the members
animalCollection.put(cat1)

// Covariance of the data structure itself
val animalCollection2: Container[Animal] = catCollection

// Won't work
// val catCollection2 : Container[Cat] = animalCollection