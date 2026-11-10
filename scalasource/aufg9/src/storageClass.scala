class Storage[T]:
  private var _contents: List[T] = List()
  def getContent: List[T] = _contents
  def addItem(item: T): Unit =
    _contents = item +: _contents
  def removeItem(itemType: T): Unit =
    _contents = _contents.filterNot(_ == itemType)
  def contains(item: T): Boolean =
    _contents.contains(item)
  def getSize: Int =
    _contents.size
  def isEmpty: Boolean =
    _contents.isEmpty
  def containedAmount(item: T): Int =
    var count = 0
    _contents.foreach(elem => if elem == item then count += 1)
    count

/*  def findItem (item: T): Option[String] =
    _contents.foreach(_contents.) */
