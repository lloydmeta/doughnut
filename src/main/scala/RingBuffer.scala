package RingBuffer

/**
 * Companion object for RingBuffer. Essentially factory methods
 */
object RingBuffer {
  /**
   * Returns a new RingBuffer with the specified max length and the new element in there
   * @param maxLength Int maximum length
   * @param element A an element
   * @tparam A
   * @return RingBuffer[A]
   */
  def apply[A](maxLength: Int)(element: A) = new RingBuffer(maxLength)(List(element))

  /**
   * Returns a RingBuffer with a given Sequence.
   *
   * The max length of the returned RingBuffer will be the same as the max length of the given
   * Sequence. Call Extend on the returned RingBuffer to make it longer.
   *
   * @param elements Seq[A]
   * @tparam A
   * @return RingBuffer[A]
   */
  def apply[A](elements: Seq[A]) = new RingBuffer(elements.length)(elements.toList)
}

/**
 * Immutable Ring Buffer. Smart people tell me that it's useful.
 *
 * Should be initialised via companion object by supplying a max length and an initial element. In short
 * a ring buffer is a FIFO data structure that has a set maximum length. Once that length is reached the oldest
 * elements get replaced by newer ones.
 *
 * @param maxLength Int
 * @param internalList List
 * @tparam A
 */
class RingBuffer[+A](val maxLength: Int)(private val internalList: List[A]) {

  lazy val reverseInternalList = internalList.reverse

  /**
   * Adds a variable number of elements to a RingBuffer returning another RingBuffer with
   * those elements in it
   *
   * @param elements [B] where B >: A
   * @tparam B
   * @return RingBuffer[B]
   */
  def add[B >: A](elements: B*): RingBuffer[B] = {
    def addOne[B >: A](element: B): RingBuffer[B] = {
      if (internalList.length == maxLength)
        new RingBuffer(maxLength)(element :: internalList.take(maxLength - 1))
      else
        new RingBuffer(maxLength)(element :: internalList)
    }
    elements.tail.foldLeft(addOne(elements.head))((m, e) => m.add(e))
  }

  /**
   * Returns a 2 element tuple (smart people tell me this is called a double)
   * with the first element being the elements that were read and the second element
   * being a new RingBuffer without the read elements
   *
   * @param oldest Int number of elements to read
   *
   * @return (List[A], RingBuffer[A]) a 2 element tuple (smart people tell me this is called a double)
   *                                  with the first element being the elements that were read and the second element
   *                                  being a new RingBuffer without the read elements
   */
  def read(oldest: Int): (List[A], RingBuffer[A]) = {
    val (oldestElements, remaining) = reverseInternalList.splitAt(oldest)
    (oldestElements, new RingBuffer(maxLength)(remaining.reverse))
  }

  /**
   * Returns a new RingBuffer with the same elements but with the max length of the buffer
   * extended
   * @param newMax Int max number of elements to read
   * @return
   */
  def extend(newMax: Int): RingBuffer[A] =  {
    require(newMax >= maxLength)
    new RingBuffer(newMax)(internalList)
  }

  override def toString = s"RingBuffer(max: $maxLength, elements: $reverseInternalList)"
}