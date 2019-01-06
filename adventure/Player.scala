package o1.adventure

import scala.collection.mutable.Map

  
/** A `Player` object represents a player character controlled by the real-life user of the program. 
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {
  private val dog = new Dog("dog", startingArea)
  private val kid = new Kid("kid", startingArea)
  
  private var currentLocation = startingArea  
  def location = this.currentLocation
  private var quitCommandGiven = false              // one-way flag
  
  private def exits = "Available exits: " + this.currentLocation.neighbors.keys.mkString(" ")
  
  var itemsCarried = Map[String, Item]() // mukana olevat tavarat
  var dependantsWith = Map[String, Dependant]() // mukana olevat huollettavat
  
  def alreadyHaveDependant(dependant: String) = this.dependantsWith.exists(_._1 == dependant)
  def alreadyHaveItem(item: String) = this.itemsCarried.exists(_._1 == item)
  
  var doNotPlayTurn = false // pelivuoro ei kulu, kun käyetään help tai commands komentoja
  
  var workDone = false
  var workOutDone = false
  var shoppingDone = false
 

  def has(itemName: String): Boolean = {
    this.itemsCarried.contains(itemName)
  }
  

  
  /* this function gives some general advice*/
  def help() = {   
    doNotPlayTurn = true
    val location = this.currentLocation.description + "\n"
    val youHaveItems = if (this.itemsCarried.isEmpty) {
      "You are not carrying anything at the moment\n"
    } else {
      "You have: " + this.itemsCarried.keys.mkString(" ") + "\n"
    }
    val youHaveDependants = if (this.dependantsWith.isEmpty) {
      "You have no dependants following you at the moment.\n"
    } else {
       "You are being followed by: " + this.dependantsWith.keys.mkString(" ")
    }
      location + youHaveItems + youHaveDependants + "\nType \"commands\" to get a list of possible commands"

  }
  
 /* get-funtiolla voi poimia esineitä*/
  def get(itemName: String): String = {
    var areaContainsItem = this.currentLocation.containsItem(itemName)
      if (areaContainsItem && !this.alreadyHaveItem(itemName) ) {
        itemsCarried.put(itemName, currentLocation.removeItem(itemName).get)
        "You pick up the " + itemName + "."
      } else if (itemName == "dog" || itemName == "kid"){
        "If you want to take one of your dependants with you, use command \"take\""
      } else {
        "There is no " + itemName + " here to pick up."
      }
  } 
  
  
  def hasItem(itemName: String): Boolean = {
    this.itemsCarried.contains(itemName)
  }
  
  def hasDependant(dependantName: String): Boolean = {
    this.dependantsWith.contains(dependantName)
  }
  

  def releaseDog(): String = {
    if (currentLocation.name == "Park1" || currentLocation.name == "Park2" ||
        currentLocation.name == "Park4" || currentLocation.name == "Park5") { // wrong parks
      "Your dog doesn't like this park, go somewhere else"
    } else if (currentLocation.name == "Home" && !this.dog.wentToPark ){
     "You need to release your dog in the park before bringing him back home"
    } else if ( this.dog.dogDone ){
      this.dependantsWith.remove(this.dog.name)
      currentLocation.addDependant(this.dog)
      "Don't you remember, you already took your dog to the park?!"
    } else if (currentLocation.name == "Park3" && !this.dog.wentToPark ){
      this.dependantsWith.remove(this.dog.name)
      currentLocation.addDependant(this.dog)
      this.dog.wentToPark = true
      "Your dog happily runs to the bushes and takes care of his morning rituals.\n ...\nOk, he is done now.\nNow you need to catch him and take him back home." 
   } else if (this.dog.wentToPark) {
       if(currentLocation.name == "Home"){
         this.dependantsWith.remove(this.dog.name)
         currentLocation.addDependant(this.dog)
         this.dog.dogDone = true
         "Great, dog has been taken care of!"
       } else {
          "You already took the dog to the park, take it home now."
       } 
   } else {
     "You cannot release your dog here."
   }
  }
  
  def releaseKid(): String = {
    if (currentLocation.name == "kindergarten" ){
      this.dependantsWith.remove(this.kid.name)
      currentLocation.addDependant(this.kid)
      this.kid.kidKindergarten = true
      "You leave your screaming kid in kindergarten\n ...\nNow you need to quickly get to your workplace.\nDont forget to pick up your kid after work..." 
    } else if (currentLocation.name == "Home"){
      if(this.kid.kidKindergarten && this.workDone && this.workOutDone && this.shoppingDone && this.hasItem("mathbook")) {
        currentLocation.addDependant(this.kid)
        this.kid.kidDone = true
        this.dependantsWith.remove(this.kid.name)
        "Good, you brought your kid back home after the day in kindergarten."
      } else {
        "You shouldn't have picked the kid up before you have done your other tasks"
      }
    } else {
      "You cannot release your kid here."
    }
  }
  
  def release(dependantName: String) = {
    if (dependantName == "dog") {
      if(this.hasDependant("dog")){
        this.releaseDog()
      } else {
        "You don't have the dog with you"
      }
    } else if (dependantName == "kid") {
      if(this.hasDependant("kid")){
        this.releaseKid()
      } else {
          "You don't have your kid with you"}
    } else {
      "Who is " + dependantName
      }    
    }   
 
  def work() = {
    if(currentLocation.name != "Workplace") {
      "You are not at your workplace!"
    } else {
      if ((!this.dog.dogDone || !this.kid.kidKindergarten) && !this.hasDependant("dog") && this.hasDependant("kid") ){
        "Did you walk the dog? \nDid you take the kid to Kindergarten?"
      } else if (this.hasDependant("dog")) {
        "Why would you bring your dog to work?"
      } else if (this.hasDependant("kid")){
        "Why would you bring your kid to work?"
      } else {
        this.workDone = true
        """You have seven meetings and ten deadlines today...
          |... You work...
          |... And work...
          |... And work some more
          |...
          |Gret! Finally you are done! Time to go to gym and grocery shopping.
          |Oh, and don't forget to pick up your child's math book from the school""".stripMargin
      }
    }
  }
  
  def workOut() = {
    if(currentLocation.name != "Gym") {
      "You are not at gym"
    } else if (!this.hasItem("gymbag")) {
      "Oh no, you forgot your gymbag!"
    } else {
      if ((!this.dog.dogDone || !this.kid.kidKindergarten || !this.workDone)&& !this.hasDependant("dog") && this.hasDependant("kid") ) {
        "Did you walk the dog? \nDid you take the kid to Kindergarten? \nDid you do go to work?" 
      } else if (this.hasDependant("dog")) {
        "Why would you bring your dog to gym?"
      } else if (this.hasDependant("kid")){
        "Why would you bring your kid to gym?"
      } else {
        this.workOutDone = true
        "You do your workout \nAfter one hour of sweating, you feel like a new person!!\n...\n...\nNow it's time to move on, you still have things to do!"
      }
    }
  }
 
  def buy(food: String) = {
    if(currentLocation.name != "Supermarket" || food != "food") {
      "Sorry, " + food + " is not sold here."
    } else if(this.dependantsWith.contains("dog")){
      "The shopkeeper refuses to sell you " + food + ", because you brought the dog into the store!"
    } else if(!this.workDone){
        "You should go to work before doing your grocery shopping."
    } else if(this.shoppingDone){
        "Don't you remember, you did your shopping already!?"
    } else {
      this.get(food)
      val bought = itemsCarried("food")
      this.shoppingDone = true
      "You buy " + bought.description
      
    }
  }   
   
  def commands() = {
    doNotPlayTurn = true
    """Try using following commands to survive the day: 
     |--------------------------------------------------------
     |buy (food)-------------------------- to buy food
     |go (direction) --------------------- to move to another area
     |get (groceries, mathbook, gymbag) -- to get an item
     |release (dependant) ---------------- to release the dependant that is with you
     |rest ------------------------------- to rest                              - 
     |take (dependant)-------------------- to take one of your dependants with you
     |work ------------------------------- to do your work
     |workout ---------------------------- to do your gym workout
     |""".stripMargin
  }
  
    
  def take(dependant: String) = {
    var areaContainsDependant = this.currentLocation.containsDependant(dependant) || this.currentLocation.containsItem(dependant)
    if (!this.alreadyHaveDependant(dependant) && areaContainsDependant){
      if(dependant == "dog") {
        this.dependantsWith.put(dog.name, dog)
        "You have the dog now on leash.\n" + this.exits
      } else if (dependant == "kid"){
         this.dependantsWith.put(kid.name, kid)
        "You struggle to get your toddler into outdoor clothing and hold her hand firmly, time to go now!\n" + this.exits
      } else if (dependant == "mathbook" || dependant == "gymbag"){
        "Use the command \"get\" instead"
      } else {
        "Who is " + dependant + "?"
      }
    } else {
      if (this.alreadyHaveDependant(dependant)){
        "You already have the " + dependant + " with you."
      } else if (!areaContainsDependant){
        "No " + dependant + " here..."
      } else {
        "What?"
      }
    }
     
  } 
    
  
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven
  

  /** Attempts to move the player in the given direction. This is successful if there 
    * is an exit from the player's current location towards the direction name. 
    * Returns a description of the results of the attempt. */

  def go(direction: String) = {
    if (this.dependantsWith.contains("kid") && this.dependantsWith.contains("dog")) {
      "Please, don't take both kid and dog with you, that would be too much!!"
    } else {
      val destination = this.location.neighbor(direction)
      this.currentLocation = destination.getOrElse(this.currentLocation) 
      if (destination.isDefined) "You go " + direction + ".\n" + this.currentLocation.description else "You can't go " + direction + "."
    }
    
  }
  
  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though." 
  }
  
  
  /** Signals that the player wants to quit the game. Returns a description of what happened within 
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  
  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name   


}


