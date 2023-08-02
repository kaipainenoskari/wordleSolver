import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Random
import scala.math.ceil

object Solver extends App {
    var newGame = true
    while newGame do
        val rand = Random()

        var lang = readLine("Select language fi/en\n").toLowerCase()
        while lang != "en" && lang != "fi" do
            lang = readLine("Select language fi/en\n").toLowerCase()

        var wordList = Vector[String]()
        var lengthString = readLine("Select length\n")
        while !lengthString.forall(_.isDigit) do
            lengthString = readLine("Select length\n")
        val wordLength = lengthString.toInt

        if lang == "en" && wordLength == 5 then
            wordList = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/wordle-answers-alphabetical.txt").getLines.toVector
            .map(word => word.toLowerCase())
            .filter(word => word.length == wordLength))
        else if lang == "en" then
            wordList = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/words_alpha.txt").getLines.toVector
            .map(word => word.toLowerCase())
            .filter(word => word.length == wordLength))
        else
            wordList = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/kaikki-suomen-sanat.txt").getLines.toVector
            .map(string => string.toLowerCase.filter(char => char.toInt != 227 && char != '-')
            .map(char => if char.toInt == 164 || char.toInt == 8222 then 'A'
                         else if char.toInt == 182 || char.toInt == 8211 then 'O' else char))
            .filter(word => word.length == wordLength))

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
                                val sb = new StringBuilder(word)
                                _indexOfBlanks
                                .forall((index, charSet) =>
                                    charSet.forall(char =>
                                        !word.contains(char) || 
                                        (!_indexOfGreens
                                        .filter(x => 
                                            for (i <- 
                                            _indexOfGreens
                                            .filter(x => 
                                                x._2.contains(char))) { sb(i._1) = '.' };
                                            x._2.contains(char)).isEmpty && 
                                                    !sb.contains(char)) ||
                                                    (word(index) != char &&
                                                    !_indexOfYellows
                                                    .filter(x => 
                                                        x._2.contains(char)).isEmpty)) ))

            return filteredWords


        def permutations(word: Seq[String]): Seq[String] =
            // Call the inner function recursively to get all the permutations of "g", "y", " "
            // there will be 3^n permutations where n == wordlength
            
            def inner(start: String, i: Int): Seq[String] =
                //println(start)
                //println(i)
                var r = List[String]()
                if i == wordLength then
                    for l <- word do
                        r = r.appended(start + l)
                else
                    for l <- word do
                        r = r ++ inner(start + l, i + 1)
                return r
            return inner("", 1)

        val permutationSet = permutations((Vector("g", "y", " "))).toVector

        def getAllIndices(word: String, char: Char): Seq[Int] =
            var indices = List[Int]()
            var subString = word
            var i = word.indexOf(char)
            var j = 0
            while i != -1 do
                j += i
                subString = subString.substring(i + 1)
                indices = indices :+ j
                j += 1
                i = subString.indexOf(char)
            indices

        def minimizePermutations(word: String): Seq[String] =
            var perm = permutationSet

            perm = perm.filter(p =>
                indexOfGreens.forall((index, charSet) =>
                    charSet.forall(char => (p(index) == 'g' && word(index) == char) || (p(index) != 'g' && word(index) != char)))
                && indexOfYellows.forall((index, charSet) =>
                    charSet.forall(char => (word(index) != char) || (p(index) != 'g' && word(index) == char)))
                && indexOfBlanks.forall((index, charSet) =>
                    charSet.forall(char => word(index) != char || (word(index) == char && p(index) == ' ') )))
            perm.filter(permutation => word.forall(char => !indexOfBlanks.values.toList.flatten.toSet.contains(char) || (getAllIndices(word, char).forall(index => permutation(index) == ' ') || (indexOfGreens.values.toList.flatten.contains(char) || indexOfYellows.values.toList.flatten.contains(char) ))))


        // todo: function for determining the best guess aka guess that minimizes the amount of correct answers.
        def bestGuess(wordList: Seq[String]): Unit =
            var sizes = scala.collection.mutable.Map[String, Vector[Int]]()
            var sizeAverages = scala.collection.mutable.Map[String, Double]()
            var validWords = Vector[String]()
            var wordFilterTime = 0L
            var permutationTime = 0L
            var everythingElse = System.nanoTime()
            var printTime = 0L

            if lang == "en" && wordLength == 5 then
                validWords = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/valid-wordle-words.txt")
                .getLines.toVector.filter(word => word.length == wordLength))
            else if lang == "en" then
                validWords = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/words_alpha.txt")
                .getLines.toVector.filter(word => word.length == wordLength))
            else
                validWords = rand.shuffle(Source.fromFile("C:/Users/oskar/wordlesolver/src/utils/kaikki-suomen-sanat.txt")
                .getLines.toVector.map(string => string.toLowerCase.filter(char => char.toInt != 227 && char != '-').map(char => if char.toInt == 164 || char.toInt == 8222 then 'A' else if char.toInt == 182 || char.toInt == 8211 then 'O' else char))
                .filter(word => word.length == wordLength))
            val startSize = validWords.length.toDouble

            while validWords.nonEmpty do
                val p1 = System.nanoTime()
                val current_word = validWords.head
                val sizeNow = validWords.length
                val percent = math.ceil(((startSize - sizeNow) / startSize) * 100).toInt
                val progress = "#" * percent
                val toRun = "-" * (100 - percent)
                print("\r[%s%s] %d %%".format(progress, toRun, percent))
                printTime += System.nanoTime() - p1
                
                val t1 = System.nanoTime()
                val perm = if (indexOfGreens.isEmpty && indexOfYellows.isEmpty && indexOfBlanks.isEmpty) then
                    permutationSet else
                    minimizePermutations(current_word).toVector
                permutationTime += System.nanoTime() - t1

                var i = 0
                while i < perm.length do
                    val t1 = System.nanoTime()
                    val currentSize = filterWords(wordList, current_word, perm(i)).length
                    wordFilterTime += System.nanoTime() - t1
                    if currentSize != 0 then
                        sizes(current_word) = sizes.get(current_word) match
                            case Some(value) => value :+ currentSize
                            case None => Vector(currentSize)
                    i += 1
                val candidateSize = if sizes.keySet.contains(current_word) then sizes(current_word).max else Int.MaxValue
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
            //printTime -= wordFilterTime + permutationTime

            everythingElse = System.nanoTime() - everythingElse - wordFilterTime - permutationTime - printTime

            println(s"\nFilter: ${(wordFilterTime / 1e9d).round} s")
            println(s"Permutation: ${(permutationTime / 1e9d).round} s")
            println(s"Printing: ${(printTime / 1e9d).round} s")
            println(s"Everything else: ${(everythingElse / 1e9d).round} s")
            println(s"Summed: ${((wordFilterTime + permutationTime + everythingElse + printTime) / 1e9d).round}s")

            println("\nAverages: " + sizeAverages.toVector
            .map((word, average) => if wordList.contains(word) then
                                    (word, average - (1/wordList.length.toDouble)) else
                                    (word, average)).sortBy(_._2).take(10)
                                    .map((word, average) => (word, (average * 100).round / 100.0)).mkString(", "))
            //sizes.map((word, list) => (word, list.max)).minBy(_._2)._1

        var truth = true
        while truth do
            var guess = readLine("guess\n")
            if guess == "exit" then
                truth = false
                newGame = false
            else if guess == "list" then
                println(wordList)
            else if guess == "answer" then
                if wordList.length == 1 || wordList.length == 2 then
                    println(rand.shuffle(wordList).head)
                else
                    val t1 = System.nanoTime()
                    println("Running...")
                    bestGuess(wordList)
                    val duration = (System.nanoTime() - t1) / 1e9d
                    println("Duration " + duration.toInt + "s")
            else if guess.length == wordLength && !guess.contains(' ') then
                var colors = readLine("colors\n").toLowerCase()
                while colors.length != wordLength && colors.filter(char => char != 'g' || char != 'y' || char != ' ').length != 0 do
                    println("Incorrect input...")
                    colors = readLine("colors\n").toLowerCase()
                if colors == "g" * wordLength then
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