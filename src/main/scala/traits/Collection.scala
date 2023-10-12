package de.uni_saarland.cs.se
package traits

import scala.annotation.tailrec


/**
 * Interface for the collection implementations and traits
 *
 * Keep in mind that the priority feature needs an implicit parameter for the `Ordering` object:
 *   class Foo[A](implicit val ordering: Ordering[A]) extends Collection[A]
 *
 * @tparam A the type of elements in the collection
 */
trait Collection[A] {
  def push(element: A): Boolean

  def pop(): Option[A]

  def peek(): Option[A]

  def size: Int = 0
}

/**
 * this interface is there to help by tracking the changing that happens in the list after every push/pop.
 * i didn't find any statement from the SE Teams that forbids adding such an Interface in Task c) .
 * this will keep the traits classes clean from any private methods((to track the elements))  like sub b).
 * @tparam A the type of elements in the collection
 */
abstract class AbstractCollection[A] extends Collection[A]{
  var elements : List[A] = Nil
  def get() : List[A]={
    elements
  }
}


/**
 * FIFOCollection
 * @tparam A the type of elements in the collection
 */
 class FIFOCollection[A] extends AbstractCollection[A]{

    override def push(element: A): Boolean = {
      elements = element :: elements
      true
    }

    override def pop(): Option[A] = {
      if (elements == Nil) {
        return None
      }
      val head: Option[A] = Some(elements.reverse.head)
      elements = elements.reverse.tail.reverse
      head
    }

    override def peek(): Option[A] = {
      if (elements == Nil) {
        return None
      }
      val head: Option[A] = Some(elements.reverse.head)

      head
    }

    override def size: Int = elements.size


}

/**
 * LIFOCollection
 * @tparam A the type of elements in the collection
 */
  class LIFOCollection[A] extends AbstractCollection[A]{

  override def push(element: A): Boolean = {
    elements = element :: elements
    true
  }

  override def pop(): Option[A] = {
    if(elements== Nil) {
      return None
    }
    val head: Option[A] = Some(elements.head)
    elements = elements.tail
    head
  }

  override def peek(): Option[A] = {
    if(elements ==Nil) {
      return None
    }
    val head: Option[A] = Some(elements.head)

    head
  }

  override def size: Int = elements.size

}

/**
 * PriorityCollection
 * @param ordering ordering
 * @tparam A the type of elements in the collection
 */
 class PriorityCollection[A] (implicit val ordering: Ordering[A]) extends AbstractCollection[A]{

  override def push(element: A): Boolean = {
    def insert(e: A, l: List[A]):List[A] = {
      l match {
        case ::(head,next)=>
          if (ordering.lteq(e, head)){
            element :: l
          } else {
            head :: insert(e, next)
          }
        case Nil => e :: Nil
      }
    }
    elements =insert(element , elements)
    true
  }

  override def pop(): Option[A] = {
    if(elements== Nil) {
      return None
    }
    val head: Option[A] = Some(elements.head)
    elements = elements.tail
    head
  }

  override def peek(): Option[A] = {
    if(elements ==Nil) {
      return None
    }
    val head: Option[A] = Some(elements.head)

    head
  }

  override def size: Int = elements.size

}

/**
 * a trait for the Capacity feature
 * @tparam A the type of elements in the collection
 */
trait Capacity[A] extends AbstractCollection[A]{

  val capacity : Int

  override abstract def push(element: A): Boolean = {
   if (super.size < capacity){
    return   super.push(element)
   }
    false
 }

  override abstract def get(): List[A] = super.get()

}

/**
 * a trait for the Uniqueness feature
 * @tparam A the type of elements in the collection
 */
trait Uniqueness[A] extends AbstractCollection[A]{

  override abstract def get(): List[A] = super.get()

  override abstract def push(element: A): Boolean = {
    @tailrec
    def hasElement(e:A,l: List[A]):Boolean = {
      l match {
        case Nil => false
        case list =>
          if (e == list.head){
            true
          } else {
            hasElement(e,list.tail)
          }
      }
    }
    if(hasElement(element,super.get())){
      return false
    }
    val result : Boolean =super.push(element)
    result
  }
}

/**
 *  a trait for the Logging feature
 * @tparam A the type of elements in the collection
 */
trait Logging[A] extends AbstractCollection[A]{

  override abstract def get(): List[A] = super.get()

  override abstract def push(element: A): Boolean = {
    val success : Boolean = super.push(element)
    if (success){
      println(s"Pushing element $element.")
    }else{
      println("Failed to push element")
    }
    success
  }

  override abstract def pop(): Option[A] = {
    if(super.get() == Nil) {
      println("Collection is empty.")
      return None
    }
    val head: Option[A] = super.pop()
    println(s"Popping element ${head.get}.")
    head
  }

  override abstract def peek(): Option[A] = {
    if(super.get() == Nil) {
      println("Collection is empty")
      return None
    }
    val head: Option[A] = super.peek()
    println(s"Peeking element ${head.get}.")
    head
  }

}
