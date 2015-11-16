val Routine =
  "0 1\n" +
    "A\n" +
    "A 0 1 R B\n" +
    "A 1 1 L B\n" +
    "B 0 1 L A\n" +
    "B 1 0 L C\n" +
    "C 0 1 R F\n" +
    "C 1 1 L D\n" +
    "D 0 1 R D\n" +
    "D 1 0 R A\n" +
    "0"

type Instruction = Map[String, (String,String,String)]

def tokenize(l:List[String]) = l match{
  case List(a,b,c,d,e) => (a,b,c,d,e)
}

def createDictionary(t:(String, String, String, String, String)) = t match{
  case (cs, cl, rl, d, ns) => Map(cs ++ cl -> (rl, d, ns))
  case _ => Map("" -> ("","",""))
}

def parse(s:String):Instruction={
  val charList = s.split(" ").toList
  val tokens = tokenize(charList)
  val dictionaries = createDictionary(tokens)
  //println(dictionaries)
  dictionaries
}
def parseInstructions(l:List[String]):Instruction={
  val dictionaryList = l.map(x  => parse(x))
  dictionaryList.foldLeft[Instruction](createDictionary(("","","","","")))((map1, map2) => map1 ++ map2)
}

def evaluateDirection(position:Int, direction:String):Int = direction match {
  case "R" => position + 1
  case "L" => if(position < 1) 0 else position - 1
}

def run(currentState:String,instructionMap:Instruction,currentWord:Seq[String],position:Int,step:Int): Unit= currentState match {
  case "F" =>
    println((currentWord,position,step))
  case _ =>
    val resultTuple = instructionMap.get(currentState + (if (currentWord.isDefinedAt(position))currentWord(position).toString else "0"))
    val resultLetter = resultTuple.get._1
    val direction = resultTuple.get._2
    val newState = resultTuple.get._3
    val newPosition = evaluateDirection(position, direction)
    val newWord = if(position > currentWord.length-1 )currentWord ++ Seq(resultLetter)
                  else if(position <= 0 && direction == "L") Seq("0") ++ currentWord.updated(position,resultLetter)
                  else currentWord.updated(position,resultLetter)
    //println((newState,resultLetter,direction,newPosition,newWord))
    run(newState,instructionMap,newWord,newPosition,step+1)
}

def initialize(t:(String,String,Instruction,String)):Unit={
  val language = t._1
  val startState = t._2
  val instructionMap = t._3
  val startWord = Seq(t._4)
  val position = 0
  val step = 0
  //println("Initial:")
  //println(startState,startWord,position)
  run(startState,instructionMap,startWord,position,step)
}

val parseList = Routine.split("\n").toList
val instructions = parseList.slice(2, parseList.size-1)
val outerTuple = (parseList.head,parseList(1),parseInstructions(instructions),parseList.last)
initialize(outerTuple)

//print(outerTuple)
