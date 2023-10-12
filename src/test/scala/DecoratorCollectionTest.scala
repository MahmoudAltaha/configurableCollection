package de.uni_saarland.cs.se

import decorator._

import org.scalatest.flatspec.AnyFlatSpec

import java.io.ByteArrayOutputStream

class DecoratorCollectionTest extends AnyFlatSpec {

  "A FIFO collection" should "pop elements that were inserted first before elements that were inserted later" in {
    val fifo = new FIFOCollection[Int]()
    fifo.push(1)
    fifo.push(2)
    fifo.push(3)
    assert(fifo.pop() === Some(1))
    assert(fifo.pop() === Some(2))
    assert(fifo.pop() === Some(3))
  }

  "A LIFO collection" should "pop elements that were inserted first after elements that were inserted later" in {
    val lifo = new LIFOCollection[Int]()
    lifo.push(1)
    lifo.push(2)
    lifo.push(3)
    assert(lifo.pop() === Some(3))
    assert(lifo.pop() === Some(2))
    assert(lifo.pop() === Some(1))
  }

  "A priority collection" should "pop the smallest element first" in {
    val priority = new PriorityCollection[Int]()
    priority.push(2)
    priority.push(3)
    priority.push(1)
    assert(priority.pop() === Some(1))
    assert(priority.pop() === Some(2))
    assert(priority.pop() === Some(3))
  }

  "A collection with capacity" must "reject new elements when it is full" in {
    val fifoWithCapacity = new CapacityDecorator[Int](capacity = 2, new FIFOCollection[Int])
    assert(fifoWithCapacity.push(1))
    assert(fifoWithCapacity.push(2))
    assert(fifoWithCapacity.push(2) === false)
    assert(fifoWithCapacity.size === 2)
  }


  "A collection with uniqueness" must "reject elements that it already contains" in {
    val lifoWithUniqueness = new UniquenessDecorator[Int](new LIFOCollection[Int])

   assert(lifoWithUniqueness.push(1))
    assert(lifoWithUniqueness.push(2))
    assert(lifoWithUniqueness.push(2) === false)
    assert(lifoWithUniqueness.size === 2)
    assert(lifoWithUniqueness.push(3))
  }


  "A collection with logging" must "log calls to its functions" in {
    val stream = new ByteArrayOutputStream()
    val priorityWithLogging = new LoggingDecorator[Int](new FIFOCollection[Int])

    Console.withOut(stream) {
      priorityWithLogging.push(1)
      assert(stream.toString.startsWith("Pushing element 1."))
      stream.reset()

      priorityWithLogging.peek()
      assert(stream.toString.startsWith("Peeking element 1."))
      stream.reset()

      priorityWithLogging.pop()
      assert(stream.toString.startsWith("Popping element 1."))
      stream.reset()

      priorityWithLogging.pop()
      assert(stream.toString.startsWith("Collection is empty."))
      stream.reset()
    }
  }


  "A collection" can "be extended with multiple features" in {
    val collection = new LoggingDecorator[Int](new UniquenessDecorator[Int](new CapacityDecorator[Int](2, new
        LIFOCollection[Int])))

    assert(collection.push(1))
    assert(collection.push(1) === false)
    assert(collection.push(2))
    assert(collection.push(3) === false)
  }


}


