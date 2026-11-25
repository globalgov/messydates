# Prose in languages other than English ####
# The prose parser reads English. Prose in another language is first
# rewritten into the English forms that the parser reads, word by word and
# phrase by phrase, so that every language shares one parser, and English
# input is never changed. A new language needs only a new table below.

# The languages that `as_messydate(lang = )` accepts.
.prose_languages <- c("en", "fr")

# Checks `lang` and returns it.
check_lang <- function(lang) {
  if (!is.character(lang) || length(lang) != 1 || is.na(lang) ||
      !lang %in% .prose_languages)
    stop("`lang` must be one of ",
         paste0('"', .prose_languages, '"', collapse = ", "), ".",
         call. = FALSE)
  lang
}

# French ----------------------------------------------------------------------
# Each pattern is matched without regard to case, and the patterns apply in
# order, so a longer phrase comes before any word inside it: the era markers
# ("av. J.-C.") come before "avant", and "au plus tard" before "tard".
# The patterns are written with the accents, and also without them, as
# French text in data often loses them.
.prose_fr <- c(
  # Typographic apostrophes, and the articles and prepositions they shorten
  "\u2019" = "'",
  "\\bl'" = "the ",
  "\\bd'" = "of ",
  # Eras, before "avant" and "apr\u00e8s" below
  "\\b(?:av\\.?|avant)\\s*J\\.?-?\\s*C\\.?" = " BC",
  "\\bavant\\s+J[\u00e9e]sus-?\\s*Christ\\b" = " BC",
  "\\bavant\\s+(?:l'|the\\s+)?[\u00e8e]re\\s+commune\\b" = " BCE",
  "\\bAEC\\b" = "BCE",
  "\\b(?:ap\\.?|apr\\.?|apr[\u00e8e]s)\\s*J\\.?-?\\s*C\\.?" = " AD",
  "\\bapr[\u00e8e]s\\s+J[\u00e9e]sus-?\\s*Christ\\b" = " AD",
  # Open ranges
  "\\bau\\s+plus\\s+tard\\b" = "no later than",
  "\\bpas\\s+plus\\s+tard\\s+que\\b" = "no later than",
  "\\bau\\s+plus\\s+t[\u00f4o]t\\b" = "no earlier than",
  "\\bpas\\s+plus\\s+t[\u00f4o]t\\s+que\\b" = "no earlier than",
  "\\bavant\\b" = "before",
  "\\bapr[\u00e8e]s\\b" = "after",
  "\\bdepuis\\b" = "since",
  # Approximate and uncertain qualifiers
  "\\baux\\s+alentours\\s+(?:de|du|des)\\b" = "around",
  "\\bautour\\s+(?:de|du|des)\\b" = "around",
  "\\bapproximativement\\b" = "approximately",
  "\\benviron\\b" = "about",
  "\\benv\\." = "about",
  "\\bvers\\b" = "around",
  "\\bpeut-?\\s*[\u00eae]tre\\b" = "possibly",
  "\\bprobablement\\b" = "possibly",
  "\\bincertaine?\\b" = "uncertain",
  "\\bsuppos[\u00e9e]ment\\b" = "reportedly",
  "\\bpr[\u00e9e]tendument\\b" = "allegedly",
  # The last day of a month
  "\\b(?:le\\s+)?dernier\\s+jour\\s+(?:de|du)\\b" = "last day of",
  # Decades: "les ann\u00e9es 1990" is "the 1990s"
  "\\b(?:les\\s+)?ann[\u00e9e]es\\s+([0-9]{3}0)\\b" = "$1s",
  # Ordinals: "1er", "1re", "5e", "5\u00e8me" are "1st", "5th"
  "\\bpremier\\b" = "1st",
  "\\bpremi[\u00e8e]re\\b" = "1st",
  "\\b([0-9]{1,3})\\s*(?:er|re|[\u00e8e]re|[\u00e8e]me|e)\\b" = "$1th",
  # Centuries, weeks, and days of the year
  "\\bsi[\u00e8e]cle\\b" = "century",
  "\\bsemaine\\b" = "week",
  "\\bjour\\b" = "day",
  # Seasons, and parts of a year
  "\\bprintemps\\b" = "spring",
  "\\b[\u00e9e]t[\u00e9e]\\b" = "summer",
  "\\bautomne\\b" = "autumn",
  "\\bhiver\\b" = "winter",
  "\\b(?:au\\s+)?d[\u00e9e]but\\s+(?:de\\s+|of\\s+)?" = "early ",
  "\\b(?:au\\s+)?milieu\\s+(?:de\\s+|of\\s+)?" = "mid ",
  "\\bmi-" = "mid ",
  "\\b(?:[\u00e0a]\\s+la\\s+)?fin\\s+(?:de\\s+|of\\s+)?" = "late ",
  # Roman calendar
  "\\bcalendes\\b" = "kalends",
  # Connectives: "du 13 au 15" is "from the 13 to the 15"
  "\\bdu\\s+([0-9]{1,2})\\s+au\\s+([0-9]{1,2})\\b" = "from the $1 to the $2",
  "\\bentre\\b" = "between",
  "\\bet\\b" = "and",
  "\\bou\\b" = "or",
  # Months, full names before abbreviations
  "\\bjanvier\\b" = "january",
  "\\bf[\u00e9e]vrier\\b" = "february",
  "\\bmars\\b" = "march",
  "\\bavril\\b" = "april",
  "\\bmai\\b" = "may",
  "\\bjuin\\b" = "june",
  "\\bjuillet\\b" = "july",
  "\\bao[\u00fbu]t\\b" = "august",
  "\\bseptembre\\b" = "september",
  "\\boctobre\\b" = "october",
  "\\bnovembre\\b" = "november",
  "\\bd[\u00e9e]cembre\\b" = "december",
  "\\bjanv\\.?(?=[\\s0-9]|$)" = "january",
  "\\bf[\u00e9e]vr?\\.?(?=[\\s0-9]|$)" = "february",
  "\\bavr\\.?(?=[\\s0-9]|$)" = "april",
  "\\bjuil\\.?(?=[\\s0-9]|$)" = "july",
  "\\bsept\\.(?=[\\s0-9]|$)" = "september",
  "\\boct\\.(?=[\\s0-9]|$)" = "october",
  "\\bnov\\.(?=[\\s0-9]|$)" = "november",
  "\\bd[\u00e9e]c\\.?(?=[\\s0-9]|$)" = "december",
  # Days of the week name no extra information
  "\\b(?:lundi|mardi|mercredi|jeudi|vendredi|samedi|dimanche)\\b" = "",
  # Articles and prepositions
  "\\b(?:le|la|les)\\b" = "the",
  "\\b(?:de|du|des)\\b" = "of",
  "\\ben\\b" = "in"
)

# Roman numerals for a century: "XIXe si\u00e8cle" is "19e si\u00e8cle".
# Only numerals followed by an ordinal mark and "si\u00e8cle" are read, so a
# Roman numeral elsewhere (such as in "MDCCLXXVI") is left as it is.
roman_centuries_fr <- function(x) {
  rx <- "\\b([IVXLC]+)\\s*(?:er|e|[\u00e8e]me)\\s+(?=si[\u00e8e]cle\\b)"
  hit <- which(!is.na(x) & stringi::stri_detect_regex(
    x, rx, opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)))
  for (i in hit) {
    m <- stringi::stri_match_first_regex(
      x[i], rx, opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
    n <- suppressWarnings(as.integer(utils::as.roman(toupper(m[1, 2]))))
    if (!is.na(n))
      x[i] <- stringi::stri_replace_first_fixed(x[i], m[1, 1], paste0(n, "e "))
  }
  x
}

# Rewrites prose in `lang` into English prose that the parser reads.
# English is returned as it is.
translate_prose <- function(x, lang = "en") {
  if (lang == "en" || length(x) == 0) return(x)
  ok <- !is.na(x) & stringi::stri_detect_regex(x, "\\p{L}")
  if (!any(ok)) return(x)
  y <- stringi::stri_trans_nfc(x[ok])
  table <- switch(lang, fr = .prose_fr)
  if (lang == "fr") y <- roman_centuries_fr(y)
  y <- stringi::stri_replace_all_regex(
    y, names(table), unname(table), vectorize_all = FALSE,
    opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE))
  x[ok] <- stri_squish(y)
  x
}
