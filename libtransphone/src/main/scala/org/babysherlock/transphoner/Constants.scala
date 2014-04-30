package org.babysherlock.transphoner

import java.io.File

object Constants extends org.babysherlock.Constants {
  val IPA_CSV = TRANSPHONER_DATA_DIR + "ipa.csv"
  val ASJP_DIST_MATRIX_CONS = TRANSPHONER_DATA_DIR + "dist.asjp-phonPattern.consonants.csv"
  val ASJP_DIST_MATRIX_VOWEL = TRANSPHONER_DATA_DIR + "dist.asjp-phonPattern.vowels.csv"
  val MIELKE_IPA_ACOUSTIC_DIST_MATRIX = TRANSPHONER_DATA_DIR + "dist.ipa-mielke-acoustic.csv"
  val KATAKANA_IPA_CSV = TRANSPHONER_DATA_DIR + "katakana2ipa.csv"
  val PHONE_INVENTORY_TSV = TRANSPHONER_DATA_DIR + "phoneInventory.tsv"
  val EVALUATION_SLANGUAGE_CSV = TRANSPHONER_DATA_DIR + "evaluation-slanguage-books.csv"
  val EVALUATION_ELLIS_KEYWORDS_CSV = TRANSPHONER_DATA_DIR + "evaluation-ellis-keywords.csv"
  val LANGS = Map(
    "EN" -> Map(
      "id" -> "EN",
      // ISO 639-3 language codes (for use with uwn)
      "iso6393" -> "eng",
      "name" -> "English",
//      "dict" -> (TRANSPHONER_DATA_DIR + "enwiktionary-ipa.tsv.bz2"),
      "dict" -> (TRANSPHONER_DATA_DIR + "twl.ipa.tsv.bz2"),
      "glosses" -> (TRANSPHONER_DATA_DIR + "twl.glosses.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "en.freqList.50k.txt.bz2"),
      "syllabify" -> (TRANSPHONER_DATA_DIR + "en.syllables.yml"),
      "aoa" -> (TRANSPHONER_DATA_DIR + "en.aoa.csv.bz2"),
      "img" -> (TRANSPHONER_DATA_DIR + "twl.img.tsv"),
      "mrc" -> (TRANSPHONER_DATA_DIR + "mrc2.dct.bz2")
    ),
    "FR" -> Map(
      "id" -> "FR",
      "iso6393" -> "fra",
      "name" -> "French",
      "dict" -> (TRANSPHONER_DATA_DIR + "fr-freedict-ipa.tsv.bz2"),
      "glosses" -> (TRANSPHONER_DATA_DIR + "fr-freedict-glosses.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "fr.freqList.50k.txt.bz2"),
      "syllabify" -> (TRANSPHONER_DATA_DIR + "fr.syllables.yml"),
      "img" -> (TRANSPHONER_DATA_DIR + "fr-freedict-img.tsv")
    ),
    "FR2" -> Map(
      "id" -> "FR2",
      "iso6393" -> "fra",
      "name" -> "French-Combined",
      "dict" -> (TRANSPHONER_DATA_DIR + "fr-combined-ipa.tsv.bz2"),
      "glosses" -> (TRANSPHONER_DATA_DIR + "fr-freedict-glosses.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "fr.freqList.50k.txt.bz2"),
      "img" -> (TRANSPHONER_DATA_DIR + "fr-combined-img.tsv")
    ),
    "ZH" -> Map(
      "id" -> "ZH",
      "iso6393" -> "cmn",
      "name" -> "Mandarin",
      "dict" -> (TRANSPHONER_DATA_DIR + "zh-cedict-full.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "zh.freqList.50k.txt.bz2"),
      "tones" -> (TRANSPHONER_DATA_DIR + "zh.tones.csv"),
      "img" -> (TRANSPHONER_DATA_DIR + "zh-cedict-img.tsv"),
      "segmenter" -> "dict",
      "lmfile" -> (EXT_DATA_DIR + Seq("nlp", "berkleylm", "bookngrams", "chi-sim.blm.gz").mkString(File.separator)),
      "lmvocab" -> (EXT_DATA_DIR + Seq("nlp", "berkleylm", "bookngrams", "chi-sim.vocab_cs.gz").mkString(File.separator)),
//  TOO BIG
//      "lmfile" -> (DATA_DIR + Seq("nlp", "berkleylm", "webngrams", "zh.blm.gz").mkString(File.separator)),
//      "lmvocab" -> (DATA_DIR + Seq("nlp", "berkleylm", "webngrams", "zh.vocab_cs.gz").mkString(File.separator)),
      // TODO: Change to script family
      "nonLatinScript" -> "true"
    ),
    "JA" -> Map(
      "id" -> "JA",
      "iso6393" -> "jpn",
      "name" -> "Japanese",
      "dict" -> (TRANSPHONER_DATA_DIR + "ja-edict-ipa.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "ja.freqList.45k.txt.bz2"),
      "img" -> (TRANSPHONER_DATA_DIR + "ja-edict-img.tsv"),
      "segmenter" -> "dict",
      // TODO: Change to script family
      "nonLatinScript" -> "true"
    ),
    "DE" -> Map(
      "id" -> "DE",
      "iso6393" -> "deu",
      "name" -> "German",
      "dict" -> (TRANSPHONER_DATA_DIR + "de-freedict-ipa.tsv.bz2"),
      "glosses" -> (TRANSPHONER_DATA_DIR + "de-freedict-glosses.tsv.bz2"),
      "freq" -> (TRANSPHONER_DATA_DIR + "de.freqList.50k.txt.bz2"),
      "img" -> (TRANSPHONER_DATA_DIR + "de-freedict-img.tsv")
  )
  )
}
