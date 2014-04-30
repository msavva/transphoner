# Transphoner #

Given sequence of input words in one language (source), and a target language, outputs
a sequence of words in the target language that

1. closely approximate the phonetics of the source words
2. has high imageability (i.e. can be visualized - typically concrete nouns)
3. is semantically related to the source words

## Usage ##
    val transphonerOptions = TransPhonerOptions(source = 'EN', target = 'EN')
    val transphoner = TransPhoner(transphonerOptions)
    val results = transphoner.transphone("water balloon")

## Web Service ##
1. Start by running `play run` in root directory
2. Go to http://localhost:9000/transphoner
3. If you want to have translations for terms in the lineup function, you need to obtain Bing API authorization keys from http://www.bing.com/dev/en-us/translator and provide them in app/models/Auth.scala

## Dictionaries ##
- English - scrabble (twl) words from [dictionary.com](http://dictionary.reference.com/)
- Chinese (Mandarin) - from [CEDICT](http://www.mdbg.net/chindict/chindict.php?page=cedict)
- French - from [freedict](http://www.freedict.org/en/)
- German - from [freedict](http://www.freedict.org/en/)
- Japanese - from [EDICT](http://www.csse.monash.edu.au/~jwb/edict.html)

All dictionaries have for each word
- native orthographies
- IPA (pronunciation) with syllable breaks and stress marks
- Word frequency

## Phonetic Similarity ##

Computes the phonetic similarity between phones, and sequences of phones (i.e. words) using the ALINE phonetic similarity metric.

## Imageability ##

Predicts the imageability of a given word from features such as part of speech using a trained linear regression model. See paper for more details.

## Semantic Similarity ##

Computes the semantic similarity between two word concepts using the WordNet lexical ontology and a distance metric defined over its tree structure.

## TODO ##
1. Better OOV handling
 - Lemmatization and simple affix pronunciation rules
 - Learn transformation from word orthography to IPA phonetic transcription (CRF model)
2. Language modeling for longer phrases
3. Use word embeddings for semantic similarity
4. Evaluation of individual components
