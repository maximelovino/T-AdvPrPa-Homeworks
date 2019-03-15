def patFoo(x: Any): Boolean = x match {
  case a: Int if a % 4 == 0 => true
  case b: String if b.length == 1 && b.charAt(0).isUpper => true
  case b: Char if b.isUpper => true
  case _: Boolean => true
  case _ => false
}

patFoo(3)
patFoo(4)
patFoo("hello")
patFoo("Hello")
patFoo(true)
patFoo(false)
patFoo(3.1)

def insert(elem: Int, l: List[Int]): List[Int] = {
  l match {
    case Nil => elem :: Nil
    case head :: tail => {
      if (elem < head) {
        elem :: l
      } else {
        head :: insert(elem, tail)
      }
    }
  }
}

def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil else insert(xs.head, isort(xs.tail))

insert(3, Nil)
insert(3, 4 :: Nil)
insert(3, 2 :: Nil)

isort(List(7, 3, 9, 2))
