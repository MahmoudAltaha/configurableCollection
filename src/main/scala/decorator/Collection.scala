package de.uni_saarland.cs.se
package decorator

import scala.annotation.tailrec


/**
 * Interface for the collection components and decorators
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
 * Superclass for the concrete components.
 *
 * @param ordering needed for the priority feature so that one can compare values in the collection;
 *                 To properly handle this parameter, subclasses should be declared as such:
 *                   class Foo[A : Ordering] extends AbstractCollection[A] {...}
 * @tparam A the type of elements in the collection
 */
abstract class AbstractCollection[A](implicit val ordering: Ordering[A]) extends Collection[A] {
  protected var elements: List[A] = Nil
}


/**
 *  FIFOCollection
 * @tparam A the type of elements in the collection
 */
class FIFOCollection[A: Ordering] extends AbstractCollection [A] {

  override def push(element: A): Boolean = {
      elements = element :: elements
      true
    }

  override def pop(): Option[A] = {
    if(elements== Nil) {
      return None
    }
    val head: Option[A] = Some(elements.reverse.head)
      elements = elements.reverse.tail.reverse
    head
  }


  override def peek(): Option[A] = {
    if(elements ==Nil) {
      return None
    }
    val head: Option[A] = Some(elements.reverse.head)

    head
  }

  override def size: Int = elements.size

  /**
   * this methode is needed in the subclasses of this class to keep them aware of the changing that happens in the list
   * @return List[A]
   */
  def get(): List[A]={
    elements
  }

}

/**
 *   LIFOCollection
 * @tparam A the type of elements in the collection
 */
class LIFOCollection[A: Ordering] extends AbstractCollection [A] {

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

  /**
   * this methode is needed in the subclasses of this class to keep them aware of the changing that happens in the list
   * @return List[A]
   */
  def get(): List[A]={
    elements
  }

}

/**
 *  PriorityCollection
 * @tparam A the type of elements in the collection
 */
class PriorityCollection[A: Ordering] extends AbstractCollection [A] {
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

  /**
   * this methode is needed in the subclasses of this class to keep them aware of the changing that happens in the list
   * @return List[A]
   */
  def get(): List[A]={
    elements
  }
}

/**
 *  Decorator
 * @param c  abstractCollection
 * @tparam A the type of elements in the collection
 */
abstract class Decorator[A :Ordering]( val c: AbstractCollection[A]) extends AbstractCollection [A]{
  val delegator : AbstractCollection[A]

}


/**
 *  CapacityDecorator
 * @param capacity capacity
 * @param c abstractCollection
 * @tparam A the type of elements in the collection
 */
class CapacityDecorator[A:Ordering](val capacity : Int, override val c: AbstractCollection[A]) extends Decorator[A](c) {

  override val delegator: AbstractCollection[A] =  c
  /**
   * this methode helps by tracking the element of the superClass used after every push/pop
   */
  private def updateElements(): Unit ={
    c match {
      case _: FIFOCollection[A] =>
        elements = c.asInstanceOf[FIFOCollection[A]].get()
      case _: LIFOCollection[A] =>
        elements = c.asInstanceOf[LIFOCollection[A]].get()
      case _ : PriorityCollection[A] =>
        elements = c.asInstanceOf[PriorityCollection[A]].get()
    }
  }

  override def push(element: A): Boolean = {

      if(capacity > c.size ){
       val  result : Boolean = c.push(element)
        this.updateElements()
        return result
      }
     false
    }

  override def pop(): Option[A] = {
   val result : Option[A] = c.pop()
    this.updateElements()
    result
  }

  override def peek(): Option[A] = {
    c.peek()
  }

  override def size: Int = c.size

  /**
   * this methode is needed in the subclasses of this class to keep them aware of the changing that happens in the list
   * @return List[A]
   */
  def get(): List[A]={
    this.updateElements()
    elements
  }


}

/**
 *  UniquenessDecorator
 * @param c abstractCollection
 * @tparam A the type of elements in the collection
 */
class UniquenessDecorator[A:Ordering](override val c: AbstractCollection[A]) extends Decorator[A](c){
  override val delegator: AbstractCollection[A] =  c
  /**
   * this methode helps by tracking the element of the superClass used after every push/pop
   */
  private def updateElements(): Unit ={
    c match {
      case _: FIFOCollection[A] =>
        this.elements = c.asInstanceOf[FIFOCollection[A]].get()
      case _: LIFOCollection[A] =>
        this.elements = c.asInstanceOf[LIFOCollection[A]].get()
      case _ : PriorityCollection[A] =>
        this.elements = c.asInstanceOf[PriorityCollection[A]].get()
      case _: CapacityDecorator[A] =>
        this.elements = c.asInstanceOf[CapacityDecorator[A]].get()
    }
  }


  override def push(element: A): Boolean = {
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
      if(hasElement(element,elements)){
        return false
    }
        val result : Boolean =c.push(element)
    this.updateElements()
    result
  }

  override def pop(): Option[A] = {
    val result: Option[A] = c.pop()
    this.updateElements()
    result
  }

  override def peek(): Option[A] = {
    c.peek()
  }

  override def size: Int = c.size

  /**
   * this methode is needed in the subclasses of this class to keep them aware of the changing that happens in the list
   * @return List[A]
   */
  def get(): List[A]={
    this.updateElements()
    elements
  }
}


/**
 *  LoggingDecorator
 * @param c abstractCollection
 * @tparam A the type of elements in the collection
 */
class LoggingDecorator[A:Ordering](override val c: AbstractCollection[A]) extends Decorator[A](c) {
  override val delegator: AbstractCollection[A] =  c
  /**
   * this methode helps by tracking the element of the superClassused after every push/pop
   */
  private def updateElements(): Unit ={
    c match {
      case _: FIFOCollection[A] =>
        this.elements = c.asInstanceOf[FIFOCollection[A]].get()
      case _: LIFOCollection[A] =>
        this.elements = c.asInstanceOf[LIFOCollection[A]].get()
      case _ : PriorityCollection[A] =>
        this.elements = c.asInstanceOf[PriorityCollection[A]].get()
      case _: CapacityDecorator[A] =>
        this.elements = c.asInstanceOf[CapacityDecorator[A]].get()
      case _: UniquenessDecorator[A] =>
        this.elements = c.asInstanceOf[UniquenessDecorator[A]].get()
    }
  }

  override def push(element: A): Boolean = {
    val success : Boolean = c.push(element)
    if (success){
     this.updateElements()
      println(s"Pushing element $element.")
    }else{
      println("Failed to push element")
    }
    success
  }

  override def pop(): Option[A] = {
    if(elements == Nil) {
        println("Collection is empty.")
      return None
    }
    val head: Option[A] = c.pop()
      println(s"Popping element ${head.get}.")
    this.updateElements()
    head
  }

  override def peek(): Option[A] = {
    if(elements == Nil) {
      println("Collection is empty")
      return None
    }
    val head: Option[A] = c.peek()
    println(s"Peeking element ${head.get}.")
    head
  }

  /**
   * according to the Assistant in the video there is no subclasses for this one , so we don't need a get() methode here
   */

}

