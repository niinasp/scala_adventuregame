package o1.adventure

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world 
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building, 
  * an acre of forest, or something completely different. What different areas have in 
  * common is that players can be located in them and that they can have exits leading to 
  * other, neighboring areas. An area also has a name and a description. 
  * @param name         the name of the area 
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String) {
  
  val neighbors = Map[String, Area]()
  var dependantsInArea = Map[String, Dependant]()
  private var itemsInArea = Map[String, Item]()
  
  def addItem(item: Item) = {
    this.itemsInArea(item.name) = item
    /* tai:
     * this.itemsInArea.put(item.name, item)
     */
  }
  
    def addDependant(dependant: Dependant) = {
    this.dependantsInArea(dependant.dependantName) = dependant
  }
    
   def containsItem(itemName: String) = this.itemsInArea.contains(itemName)
   
   def containsDependant(dependantName: String) = this.dependantsInArea.exists(_._1 == dependantName)
  
   
  
  /** Returns the area that can be reached from this area by moving in the given direction. The result 
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  
  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in 
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }

  
  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling 
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }
  
  
  /** Returns a multi-line description of the area as a player sees it. This includes a basic 
    * description of the area as well as information about exits and items. */
  def fullDescription = {
    val exitList = "\n\nAvailable exits: " + this.neighbors.keys.mkString(" ")
    val itemsSeen = if (this.itemsInArea.keys.size > 0) {"\nYou see here: " + this.itemsInArea.keys.mkString(" ")} else {""}
    val dependantsSeen = if (this.dependantsInArea.keys.size > 0) {"\nAlso here is/are: " + this.dependantsInArea.keys.mkString(" ")} else {""}
    this.description + "\n" + itemsSeen + dependantsSeen + exitList + "\nType \"help\" if you don't know what to do"
  }
  /* tai:
    def fullDescription = {
    val contentsList = if (this.contents.isEmpty) "" else "\nYou see here: " + this.contents.values.mkString(" ")
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    this.description + contentsList + exitList
    * 
    */
  
  def removeItem(itemName: String): Option[Item] = {
    this.itemsInArea.remove(itemName)
  }
  
  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

  
  
}
