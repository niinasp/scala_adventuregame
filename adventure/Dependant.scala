package o1.adventure

/** The class `Dependant` represents beings that depend on the player of the came. Each dependant has a name 
  * and a starting location 
  
  * @param name         the dependant's name */
 
class Dependant (val name: String, startingArea: Area) {
  val dependantName = this.name
  var currentLocation = startingArea 
  
}

class Dog(name: String, startingArea: Area) extends Dependant(name, startingArea) {
  var wentToPark = false
  var dogDone = false 
  def isInWrongPark = this.currentLocation.name == "park1" || this.currentLocation.name == "park2"
}

class Kid(name: String, startingArea: Area) extends Dependant(name, startingArea) {
  var kidKindergarten = false
  var kidDone = false
  
 }


