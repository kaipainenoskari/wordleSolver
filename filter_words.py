def filterWords(words, guess, colors):
    indexOfGreens = findIndex(colors, 'g')
    
    indexOfYellows = findIndex(colors, 'y')

    indexOfBlanks = findIndex(colors, ' ')

    print(words)
    print(guess)
    print(colors)
    print(indexOfGreens)
    print(indexOfYellows)
    print(indexOfBlanks)
    for i in indexOfGreens:
        print(guess[i])
        words = filter(lambda word: word[i] == guess[i], words)
    
    for i in indexOfYellows:
        print(guess[i])
        words = filter(lambda word: word[i] != guess[i] and guess[i] in word, words)

    for i in indexOfBlanks:
        print(guess[i])
        words = filter(lambda word: guess[i] not in word, words)

    print(list(words))
    return list(words)
    

def findIndex(s, ch):
    return [i for i, ltr in enumerate(s) if ltr == ch]
