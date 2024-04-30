from googletrans import Translator

def translate_to_english(sentence):
    # Create an instance of the Translator class
    translator = Translator()

    # Detect the language of the input text
    result = translator.translate(sentence, dest='en')

    # Return the translated text
    return result.text

# Example of use
if __name__ == "__main__":
    sentence = input("Write a sentence: ")
    translation = translate_to_english(sentence)
    print("Translation in English:", translation)

