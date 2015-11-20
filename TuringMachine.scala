import scala.annotation.tailrec
import scala.io.BufferedSource
/*
  Turing Machine Simulator:
  This takes in a String of the following form:
  <Word Language>
  <Blank Character>
  <Start State>
  <Final State>
  <Instruction 1>
  ...
  <Instruction N>
  <Start Word>

  and "runs" its equivalent Turing Machine.

  A machine will continue running instructions until it sees the final state after which it will output the final word
  it generated as well as its current position and the number of steps it took.

  Here's an example of an instruction:
  A 0 1 R B
  It should read is "When I'm in state A, and I see the character '0' I'll replace that '0' with a '1', go right,
  and go to state 'B'"
  The start word is simply an initial word constructed from the word language.
 */
def makeTuringMachine(Source:String){
  type Instruction = Map[String, (String,String,String)]

  def createInstruction(l:List[String] = List("")) = l match {
    case List(cs, cl, rl, d, ns) => Map(cs ++ cl ->(rl, d, ns))
    case _ => Map("" ->("", "", ""))
  }

  def parseInstructions(l: List[String]): Instruction = {
    val instructionList = l.map(x => createInstruction(x.split(" ").toList))
    instructionList.foldLeft[Instruction](createInstruction())((map1, map2) => map1 ++ map2)
  }

  val machineParameters = Source.split("\n").toList
  val instructions = machineParameters.slice(4, machineParameters.size - 1)
  val blankCharacter = machineParameters(1)
  val finalState = machineParameters(3)
  val machine = (machineParameters.head, machineParameters(2), parseInstructions(instructions), machineParameters.last)

  def evaluateDirection(position: Int, direction: String):Int = direction match {
    case "R" => position + 1
    case "L" => if (position < 1) 0 else position - 1
  }

  @tailrec
  def run(currentState: String, instructionMap: Instruction, currentWord: Seq[String], position: Int= 0, step: Int = 0): Unit = currentState match {
    case `finalState` =>
      println("Result: " ++ currentWord.mkString("") ++ "\n" ++ "Final Position: " ++ position.toString ++ "\n" ++ "Number of Steps: " ++ step.toString ++ "\n")
    case _ =>
      val resultTuple = instructionMap.get(currentState + (if (currentWord.isDefinedAt(position)) currentWord(position).toString else `blankCharacter`))
      val resultLetter = resultTuple.get._1
      val direction = resultTuple.get._2
      val newState = resultTuple.get._3
      val newPosition = evaluateDirection(position, direction)
      val newWord = if (position > currentWord.length - 1) currentWord ++ Seq(resultLetter)
      else if (position <= 0 && direction == "L") Seq(`blankCharacter`) ++ currentWord.updated(position, resultLetter)
      else currentWord.updated(position, resultLetter)
      run(newState, instructionMap, newWord, newPosition, step + 1)
  }
  run(machine._2,machine._3,machine._4.toList.map(x => x.toString).toSeq)
}
val source = scala.io.Source.fromFile("unaryadder.txt")
val lines = source.getLines().mkString("\n")
makeTuringMachine(lines)
