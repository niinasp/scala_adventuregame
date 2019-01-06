package o1.adventure


/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect, 
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim

  
  /** Causes the given player to take the action represented by this object, assuming 
    * that the command was understood. Returns a description of what happened as a result 
    * of the action (such as "You go west."). The description is returned in an `Option` 
    * wrapper; if the command was not recognized, `None` is returned. */
  def execute(actor: Player) = {                                 
    if (this.verb == "rest") {
      Some("Oh, come on! You cannot rest, you have things to do!")
    } else if (this.verb == "help") {
      Some(actor.help())
    } else if (this.verb == "quit") {
      Some(actor.quit())
    } else if (this.verb == "commands") {
      Some(actor.commands())
    } else if (this.verb == "take") {
      Some(actor.take(this.modifiers))
    } else if (this.verb == "go") {
      Some(actor.go(this.modifiers))
    } else if (this.verb == "release") {
      Some(actor.release(this.modifiers))
    } else if (this.verb == "work") {
      Some(actor.work())
    } else if (this.verb == "workout") {
      Some(actor.workOut())
    } else if (this.verb == "get") {
      Some(actor.get(this.modifiers))
    } else if (this.verb == "buy"){ 
      Some(actor.buy(this.modifiers))
    } else { 
      None
    }
    
  }


  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"  

  
}

