package o1.adventure


/** The class `Adventure` represents text adventure games. An adventure consists of a player and 
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very 
  * specific adventure game that involves a small trip through a twisted forest. All newly created 
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure 
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "Ruuhkavuosiseikkailu"
  
  private val home  = new Area("Home", "You are at home \nBut you haven't finished all your tasks")
  private val city1 = new Area("City", "You are in the city centre. \nLots of cars and people everywhere")
  private val city2 = new Area("City", "You are in the city centre. \nBe careful, you almost got hit by a bus!")
  private val city3 = new Area("City", "You are in city centre, quite close to your home.")
  private val city4 = new Area("City", "You are in the city centre. \nYou know this neighbourhood very well.")
  private val city5 = new Area("City", "You are somewhere in the city, far away from home")
  private val park1 = new Area("Park1", "You are in a park, you see joggers and dog walkers.")
  private val park2 = new Area("Park2", "You are in a park, you see a small pond.")
  private val park3 = new Area("Park3", "This is the park your dog likes!")
  private val park4 = new Area("Park4", "You are in a park you see bushes.")
  private val park5 = new Area("Park", "You found another park!")
  private val supermarket = new Area("Supermarket", "You are in a busy superhypermarket.")
  private val kindergarten = new Area("kindergarten", "You are in kindergarten.")
  private val school = new Area("School", "You are at school, a group of teenagers is staring at you \nsuspiciously from behind their smartphones.")
  private val gym = new Area("Gym", "You are at gym. You hear a lot of clanking and puffing")
  private val work        = new Area("Workplace", "You are at your workplace. Your co-workers stare at their monitors.")
  private val destination = home
  
  
  
  
  home.setNeighbors(Vector("up" -> city2, "right" -> city4, "down" -> park2, "left" -> city3   ))
  city1.setNeighbors(Vector("up" -> gym, "down" -> city4,      "left" -> city2, "right" -> park5 ))
  city2.setNeighbors(Vector("up" -> work,  "right" -> city1, "down" -> home,   "left" -> kindergarten   ))
  city3.setNeighbors(Vector("up" -> kindergarten,  "right" -> home, "down" -> park1))
  city4.setNeighbors(Vector("up" -> city1,  "right" -> supermarket, "down" -> park3, "left" -> home))
  city5.setNeighbors(Vector("down" -> park5, "left" -> gym   ))
  park1.setNeighbors(Vector("up" -> city3,      "right" -> park2  ))
  park2.setNeighbors(Vector("up" -> home, "right" -> park3, "left" -> park1))
  park3.setNeighbors(Vector("up" -> city4,    "left" -> park2 , "right" -> park4  ))
  park4.setNeighbors(Vector("up" -> supermarket, "left" -> park3))
  park5.setNeighbors(Vector("up" -> city5, "left" -> city1, "down" -> supermarket   ))
  supermarket.setNeighbors(Vector("up" -> park5,  "left" -> city4, "down" -> park4))
  kindergarten.setNeighbors(Vector("up" -> school,  "right" -> city2, "down" -> city3))
  school.setNeighbors(Vector(  "right" -> work, "down" -> kindergarten))
  gym.setNeighbors(Vector(  "left" -> work, "down" -> city1, "right" -> city5))
  work.setNeighbors(Vector(  "right" -> gym,  "left" -> school,  "down" -> city2 ))

  
  private val food = new Item("food", "Tomatoes, bread and juice")   
  private val mathbook = new Item("mathbook", "Your child's mathbook")
  private val gymBag = new Item("gymbag", "shoes, water bottle ns towel")
  private val kid = new Kid("kid", home)
  private val dog = new Dog("dog", home)
    
  
  this.school.addItem(this.mathbook)
  this.supermarket.addItem(this.food)
  this.home.addItem(this.gymBag)
  this.home.addDependant(this.kid)
  this.home.addDependant(this.dog)
  
  /** The character that the player controls in the game. */
  val player = new Player(home)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 38

  
  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = {
    this.dog.dogDone && this.kid.kidDone && this.player.hasItem("mathbook") && this.player.shoppingDone &&
    this.player.workDone && this.player.workOutDone && this.player.location == this.destination  }


  /** Determines whether the player has won, lost, or quit, thereby ending the game. */ 
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = """You wake up. 
    |It is dark Monday morning in November. 
    |*************************************
    |You feel tired after only 4 hours of sleep 
    |(you worked on your Scala exercises until very late).
    |
    |You have not seen the sun for three weeks.
    |You woder what time it is...
    |
    |Oh no, it is seven o'clock!! Your alarm did not go off...
    |
    |You are late already!!!
    |
    | You are in a hurry... You need to:
    | - walk the dog
    | - take your smaller kid to kindergarten
    | - go to work
    | - go to gym
    | - go to supermarket 
    | - your older child is sick, so you also need to pick his maths book from the school
    | 
    | Busy day ahead (as always)! 
    | ****************************""".stripMargin

    
  /** Returns a message that is to be displayed to the player at the end of the game. The message 
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete) {
      "You are a hero! You survived Monday!! You have finished all your tasks and the house is peaceful. \nNow you need to start coding..."
    } else if (this.turnCount == this.timeLimit) {
       """............................................................
         |Oh no! You did not finish your Monday tasks in a given time!
         |............................................................
         |You are a completely useless human being!
         | 
         |But well, it is Tuesday tomorrow, maybe it will be easier.
         |.....
         | Probably not...
         | Bye!""".stripMargin
    } else {
      "You give up! You decide to hibernate and try again in May..."
    }
       
  }

  
  /** Plays a turn by executing the given in-game command, such as "go left". Returns a textual 
    * report of what happened, or an error message if the command was unknown. In the latter 
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined && !(command == "help" || command == "commands")) { 
      
      this.turnCount += 1 
    } 
  outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
}
}
