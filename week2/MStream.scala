package week2

trait MStream[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: MStream[T]
  
  def filter(p: T => Boolean): MStream[T] = {
    if(isEmpty) this
    else if(p(head)) MStream.cons(head, tail.filter(p))
    else tail.filter(p)
  }
}

object MStream {
  def cons[T](hd: T, tl: => MStream[T]) = new MStream[T] {
    def isEmpty = false
    def head = hd
    
    def tail = tl //Because tl is a by-name parameter, it will not get 
    //evaluated until tl gets dereferenced, i.e., someone calls ".tail"
  }
  val empty = new MStream[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("empty.head")
    def tail = throw new NoSuchElementException("empty.tail")
  }
  
}
