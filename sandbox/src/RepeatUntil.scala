object RepeatUntil extends App {

  trait UntilClause {
    def condition: Boolean
  }

  def until(condition: => Boolean): UntilClause = new UntilClause {
    def condition = condition
  }

  def repeat(command: => Unit)(untilClause : UntilClause): Unit = {
    command
    if (untilClause.condition) ()
    else repeat(command)(untilClause)
  }

  var i = 0
  lazy val c = {
    println("condition called")
    until(i > 10)
  }
  repeat ({
    println(i)
    i += 1
  })(c)
}
