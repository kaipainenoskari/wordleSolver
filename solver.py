from filter_words import filterWords

wordle_answers_file = open("utils/wordle-answers-alphabetical.txt", "r")

wordle_answers_data = wordle_answers_file.read()

wordle_answers_list = wordle_answers_data.replace('\n', ' ').split(" ")

wordle_answers_file.close()

while True:
    guess = input("guess: \n")
    if guess == "exit":
        break
    if guess.lower() == "answer":
        print(wordle_answers_list)
        continue
    colors = input("colors: \n")
    if colors.lower() == "back":
        continue

    wordle_answers_list = filterWords(wordle_answers_list, guess, colors)

