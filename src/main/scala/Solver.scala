import scala.io.Source
import scala.io.StdIn.readLine
import java.util.Random

object Solver extends App {
    var wordList = Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/wordle-answers-alphabetical.txt").getLines.toList
    val rand = Random()

    def merge(m1: Map[Int, Set[Char]], m2: Map[Int, Set[Char]]): Map[Int, Set[Char]] =
        (m1.toSeq ++ m2.toSeq).groupBy(_._1).map(x => (x._1, x._2.flatMap(y => y._2).toSet))

    var indexOfGreens = Map[Int, Set[Char]]()
    var indexOfYellows = Map[Int, Set[Char]]()
    var indexOfBlanks = Map[Int, Set[Char]]()

    def filterWords(words: List[String], guess: String, colors: String): List[String] =
        val c = colors.toList
        indexOfGreens = merge(indexOfGreens, c.zipWithIndex.filter(x => x._1 == 'g').map(x => (x._2, Set(guess(x._2)))).toMap)
        indexOfYellows = merge(indexOfYellows, c.zipWithIndex.filter(x => x._1 == 'y').map(x => (x._2, Set(guess(x._2)))).toMap)
        indexOfBlanks = merge(indexOfBlanks, c.zipWithIndex.filter(x => x._1 == ' ').map(x => (x._2, Set(guess(x._2)))).toMap)

        println(indexOfGreens)
        println(indexOfYellows)
        println(indexOfBlanks)
        var filteredWords = words
        filteredWords = filteredWords
                        .filter(word =>
                            indexOfGreens
                            .forall((index, charSet) =>
                                charSet
                                .forall(char =>
                                    word(index) == char)))
        filteredWords = filteredWords
                        .filter(word =>
                            indexOfYellows
                            .forall((index, charSet) =>
                                charSet
                                .forall(char =>
                                    word.contains(char) && word(index) != char)))
        filteredWords = filteredWords
                        .filter(word =>
                            indexOfBlanks
                            .forall((index, charSet) =>
                                charSet.forall(char =>
                                    !word.contains(char) || 
                                    !indexOfGreens
                                    .filter(x => 
                                        x._2.contains(char)).isEmpty && 
                                        indexOfGreens
                                        .filter(x => 
                                            x._2.contains(char))
                                            .forall(y =>
                                                !((word.take(y._1) + word.drop(y._1 + 1)).contains(char))))))

        return filteredWords

    var truth = true
    while truth do
        var guess = readLine("guess \n")
        if guess == "exit" then truth = false
        else if guess.toLowerCase() == "list" then
            println(wordList)
        else
            if guess.toLowerCase() == "answer" then
                guess = wordList(rand.nextInt(wordList.size))
                println(guess)
            val colors = readLine("colors \n")

            wordList = filterWords(wordList, guess, colors)
            
            if wordList.size <= 10 then
                println(wordList)
}
