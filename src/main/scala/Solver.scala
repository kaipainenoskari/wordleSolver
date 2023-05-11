import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random
import scala.math.*

object Solver extends App {
    var lang = readLine("Select language fi/en\n").toLowerCase()
    while lang != "en" && lang != "fi" do
        lang = readLine("Select language fi/en\n").toLowerCase()

    var wordList = Vector[String]()

    if lang == "en" then
        wordList = Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/wordle-answers-alphabetical.txt").getLines.toVector.map(word => word.toLowerCase())
    else
        println("aoäö")
        println("äö".length)
        wordList = Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/kaikki-suomen-sanat.txt").getLines.toVector.map(word => word.toLowerCase()).filter(word => word.length == 5)

    val rand = Random()

    def merge(m1: Map[Int, Set[Char]], m2: Map[Int, Set[Char]]): Map[Int, Set[Char]] =
        (m1.toSeq ++ m2.toSeq).groupBy(_._1).map(x => (x._1, x._2.flatMap(y => y._2).toSet))

    var indexOfGreens = Map[Int, Set[Char]]()
    var indexOfYellows = Map[Int, Set[Char]]()
    var indexOfBlanks = Map[Int, Set[Char]]()

    def filterWords(words: Seq[String], guess: String, colors: String): Seq[String] =
        val c = colors.toList.zipWithIndex
        val _indexOfGreens = merge(indexOfGreens, c.filter(x => x._1 == 'g').map(x => (x._2, Set(guess(x._2)))).toMap)
        val _indexOfYellows = merge(indexOfYellows, c.filter(x => x._1 == 'y').map(x => (x._2, Set(guess(x._2)))).toMap)
        val _indexOfBlanks = merge(indexOfBlanks, c.filter(x => x._1 == ' ').map(x => (x._2, Set(guess(x._2)))).toMap)

        var filteredWords = words
        filteredWords = filteredWords
                        .filter(word =>
                            _indexOfGreens
                            .forall((index, charSet) =>
                                charSet
                                .forall(char =>
                                    word(index) == char)))
        filteredWords = filteredWords
                        .filter(word =>
                            _indexOfYellows
                            .forall((index, charSet) =>
                                charSet
                                .forall(char =>
                                    word.contains(char) && word(index) != char)))
        filteredWords = filteredWords
                        .filter(word =>
                            _indexOfBlanks
                            .forall((index, charSet) =>
                                charSet.forall(char =>
                                    !word.contains(char) || 
                                    (!_indexOfGreens
                                    .filter(x => 
                                        x._2.contains(char)).isEmpty && 
                                        _indexOfGreens
                                        .filter(x => 
                                            x._2.contains(char))
                                            .forall(y =>
                                                !((word.take(y._1) + word.drop(y._1 + 1)).contains(char)))) ||
                                                (word(index) != char &&
                                                !_indexOfYellows
                                                .filter(x => 
                                                    x._2.contains(char)).isEmpty)) ))

        return filteredWords


    def permutations(word: Seq[String]): Seq[String] =
        var res = List[String]()
        for a <- word do
            for b <- word do
                for c <- word do
                    for d <- word do
                        for e <- word do
                            res = res.appended(a+b+c+d+e)
        res

    def minimizePermutations(word: String): Seq[String] =
        val perm = permutations((Vector("g", "y", " "))).toVector
        perm.filter(p => indexOfGreens.forall((index, charSet) => charSet.forall(char => (p(index) == 'g' && word(index) == char) || (p(index) != 'g' && word(index) != char)))
            && indexOfYellows.forall((index, charSet) => charSet.forall(char => (word(index) != char) || (p(index) != 'g' && word(index) == char)))
            && indexOfBlanks.forall((index, charSet) => charSet.forall(char => (word(index) != char) || (p(index) == ' ' && word(index) == char))))




    // todo: function for determining the best guess aka guess that minimizes the amount of correct answers.
    def bestGuess(wordList: Seq[String]): Unit = 
        var validWords = Vector[String]()
        if lang == "en" then
            validWords = Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/valid-wordle-words.txt").getLines.toVector.filter(word => word.length == 5)
        else
            validWords = Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/kaikki-suomen-sanat.txt").getLines.toVector.filter(word => word.length == 5)
        val startSize = validWords.length.toDouble
        var sizes = scala.collection.mutable.Map[String, Vector[Int]]()
        var sizeAverages = scala.collection.mutable.Map[String, Double]()
        var minOfMax = Int.MaxValue
        while validWords.nonEmpty do
            val current_word = validWords.head
            val sizeNow = validWords.length
            print("\rRunning... %d %%".format(math.ceil(((startSize - sizeNow) / startSize) * 100).toInt))
            //println(current_word)
            
            val perm = if (indexOfGreens.isEmpty && indexOfYellows.isEmpty && indexOfBlanks.isEmpty) then permutations((Vector("g", "y", " "))).toVector else minimizePermutations(current_word).toVector
            var i = 0
            var condition = true
            while i < perm.length && condition do
                val currentSize = filterWords(wordList, current_word, perm(i)).length
                if currentSize != 0 then
                    sizes(current_word) = sizes.get(current_word) match
                        case Some(value) => value :+ currentSize
                        case None => Vector(currentSize)
                    //if currentSize > minOfMax then
                    //    condition = false
                i += 1
            val candidateSize = if sizes.keySet.contains(current_word) then sizes(current_word).max else Int.MaxValue
            if candidateSize < minOfMax then
                minOfMax = candidateSize
                /*
                println("word -> " + current_word)
                println("worst case length -> " + minOfMax)
                println("amount of permutations -> " + perm.length)
                */
                //println("permutations -> " + perm)
            if candidateSize == 1 && wordList.contains(current_word) then
                println("\n" + current_word)
                return
            sizeAverages += current_word -> sizes(current_word).sum/sizes(current_word).length.toDouble
            validWords = validWords.tail
        println("\nAverages: " + sizeAverages.toVector.sortBy(_._2).take(10).mkString(", "))
        println("\nWorst case: " + sizes.map((word, list) => (word, list.max)).toVector.sortBy(_._2).take(10).mkString(", "))
        //sizes.map((word, list) => (word, list.max)).minBy(_._2)._1

    var truth = true
    while truth do
        var guess = readLine("guess\n").toLowerCase()
        if guess == "exit" then truth = false
        else if guess == "list" then
            println(wordList)
        else if guess == "answer" then
            if wordList.length == 1 || wordList.length == 2 then
                println(rand.shuffle(wordList).head)
            else
                val t1 = System.nanoTime()
                bestGuess(wordList)
                val duration = (System.nanoTime() - t1) / 1e9d
                println("Duration " + duration.toInt + "s")
        else if guess.length == 5 then
            var colors = readLine("colors\n").toLowerCase()
            while colors.length != 5 && colors.filter(char => char != 'g' || char != 'y' || char != ' ').length != 0 do
                println("Incorrect input...")
                colors = readLine("colors\n").toLowerCase()
            if colors == "ggggg" then
                truth = false
            else
                wordList = filterWords(wordList, guess, colors).toVector

                val c = colors.toList.zipWithIndex
                indexOfGreens = merge(indexOfGreens, c.filter(x => x._1 == 'g').map(x => (x._2, Set(guess(x._2)))).toMap)
                indexOfYellows = merge(indexOfYellows, c.filter(x => x._1 == 'y').map(x => (x._2, Set(guess(x._2)))).toMap)
                indexOfBlanks = merge(indexOfBlanks, c.filter(x => x._1 == ' ').map(x => (x._2, Set(guess(x._2)))).toMap)

                /*
                println(indexOfGreens)
                println(indexOfYellows)
                println(indexOfBlanks)
                */
                
                if wordList.size <= 10 then
                    if wordList.size == 0 then
                        println("No answers match...")
                    else
                        println(wordList.mkString(", "))
        else
            println("Incorrect input...")
}
