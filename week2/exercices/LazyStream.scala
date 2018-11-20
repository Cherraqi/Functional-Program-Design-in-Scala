package week2

trait LazyStream[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: LazyStream[T]
  
  def filter(p: T => Boolean): LazyStream[T] = {
    if(isEmpty) this
    else if(p(head)) LazyStream.cons(head, tail.filter(p))
    else tail.filter(p)
  }
  def apply(n: Int): T = {
    if(n==0) head
    else tail.apply(n-1)
  }
}

object LazyStream {
  def cons[T](hd: T, tl: => LazyStream[T]) = new LazyStream[T] {
    def isEmpty = false
    def head = hd
    
    lazy val tail = tl //Because tl is a by-name parameter, it will not get 
    //evaluated until tl gets dereferenced, i.e., someone calls ".tail"
    //Additionally, lazy will not get unnecessary recomputation because 
    //we made it a lazy val
  }
  val empty = new LazyStream[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("empty.head")
    def tail = throw new NoSuchElementException("empty.tail")
  }
  
}
