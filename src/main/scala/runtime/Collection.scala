package de.uni_saarland.cs.se
package runtime

import scala.annotation.tailrec

abstract class CollectionAccess

case class FIFO() extends CollectionAccess

case class LIFO() extends CollectionAccess

case class PRIORITY() extends CollectionAccess


/**
 * Configuration object for the collection class.
 *
 * @param access the type of access to the collection
 * @param capacity the capacity of the collection or `None` for no capacity
 * @param uniqueness whether inserted elements need to be unique
 * @param logging whether to log function calls
 */
class CollectionConfig(
                        val access: CollectionAccess,
                        val capacity: Option[Int] = None,
                        val uniqueness: Boolean = false,
                        val logging: Boolean = false,
                      ) {}


/**
 * A runtime-configurable version of our collection SPL.
 *
 * @param config the configuration for the collection
 * @param ordering needed for the priority feature so that one can compare values in the collection
 * @tparam A the type of elements in the collection
 */
class Collection[A](val config: CollectionConfig)(implicit val ordering: Ordering[A]) {

  val capacity :Option [Int] = config.capacity    // this parameter  is used if capacity defined , otherwise its just there with none.

  val capacityDefined   : Boolean = config.capacity.isDefined;
  val loggingDefined    : Boolean = config.logging;
  val uniquenessDefined : Boolean = config.uniqueness;
  val priorityDefined   : Boolean = config.access.isInstanceOf[PRIORITY]
  val lifoDefined       : Boolean = config.access.isInstanceOf[LIFO]
  val fifoDefined       : Boolean = config.access.isInstanceOf[FIFO]

  private var elements : List[A] = Nil


  /**
   * this methode used to push an element into the collection
   * @param element element
   * @return true if succeed
   */
  def push(element : A): Boolean ={

    if (capacity.isDefined){
      assert(capacity.isDefined)
      if(capacity.get <= elements.size ){
        if(loggingDefined){
          println("Failed to push element")
        }
        return false
      }
    }
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
    if(uniquenessDefined){
      if(hasElement(element,elements)){
        if (loggingDefined) {
          println("Failed to push element")
        }
        return false
      }
    }

    if(loggingDefined){
      println(s"Pushing element $element.")
    }

    if(fifoDefined || lifoDefined){
      elements = element :: elements
    } else if (priorityDefined){
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
    }
    true
  }


  /**
   *  this methode used to pop an element from the collection
   * @return
   */
  def pop(): Option[A] = {
    if(elements== Nil) {
      if(loggingDefined){
       println("Collection is empty.")
      }
      return None
    }
    var head :Option[A] = None
    if(fifoDefined){
      head = Some(elements.reverse.head)
      elements = elements.reverse.tail.reverse
    }else if (lifoDefined || priorityDefined){
      head = Some(elements.head)
      elements = elements.tail
    }
    if(loggingDefined){
      println(s"Popping element ${head.get}.")
    }
    head
  }

  /**
   * this methode used to get the head element of the Collection, the element does not get deleted or removed from the collection tho.
   * @return head
   */
  def peek(): Option[A] ={

    if(elements ==Nil) {
      if (loggingDefined) {
        println("Collection is empty.")
      }
      return None
    }
    var head : Option[A] = None
    if (fifoDefined){
       head =   Some( elements.reverse.head )
    } else if (lifoDefined || priorityDefined){
     head =  Some(elements.head )
    }
    if (loggingDefined){
      println(s"Peeking element ${head.get}.")
    }
    head
  }


  /**
   *  methode to get the number of elements in the Collection
   * @return elements.size
   */
  def size(): Int ={
    if( loggingDefined){
      println(s"Current size is ${elements.size}." )
    }
    elements.size
  }



}
