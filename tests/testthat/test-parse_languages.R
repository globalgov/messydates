fr <- function(x) as.character(as_messydate(x, lang = "fr"))

test_that("lang must be a supported language", {
  expect_error(as_messydate("15 mars 2019", lang = "de"), "must be one of")
  expect_error(as_messydate("15 mars 2019", lang = c("en", "fr")), "must be one of")
  # English is the default, and French words are not read as English
  expect_identical(as_messydate("2019-03-15", lang = "en"), as_messydate("2019-03-15"))
})

test_that("French dates with written months parse", {
  expect_equal(fr("15 mars 2019"), "2019-03-15")
  expect_equal(fr("le 15 mars 2019"), "2019-03-15")
  expect_equal(fr("lundi 15 mars 2019"), "2019-03-15")
  expect_equal(fr("le 1er mars 2019"), "2019-03-01")
  expect_equal(fr("mars 2019"), "2019-03")
  # with and without accents, and abbreviated
  expect_equal(fr(c("15 f\u00e9vrier 2019", "15 fevrier 2019", "15 f\u00e9vr. 2019")),
               rep("2019-02-15", 3))
  expect_equal(fr(c("1er ao\u00fbt 1914", "15 d\u00e9c. 2019", "15 janv. 2019")),
               c("1914-08-01", "2019-12-15", "2019-01-15"))
  expect_equal(fr("mai-97"), "XX97-05")
})

test_that("French qualifiers and open ranges parse", {
  expect_equal(fr(c("vers 1910", "environ 1910", "aux alentours de 1910")),
               rep("~1910", 3))
  expect_equal(fr("peut-\u00eatre 1910"), "?1910")
  expect_equal(fr("peut-\u00eatre vers 1910"), "%1910")
  expect_equal(fr("vers le 15 mars 2019"), "2019-03-15~")
  expect_equal(fr(c("avant 1910", "apr\u00e8s 1910", "depuis 1910")),
               c("..1910", "1910..", "1910.."))
  expect_equal(fr("au plus tard le 15 mars 2019"), "..2019-03-15")
  expect_equal(fr("environ 2019-01-01..2019-01-05"), "2019-01-01~..2019-01-05~")
})

test_that("French decades, centuries, eras, and parts of a year parse", {
  expect_equal(fr(c("les ann\u00e9es 1990", "ann\u00e9es 1920")), c("199X", "192X"))
  expect_equal(fr(c("le XIXe si\u00e8cle", "le 19e si\u00e8cle", "XIX\u00e8me si\u00e8cle")),
               rep("18XX", 3))
  expect_equal(fr("le Ve si\u00e8cle av. J.-C."), "-04XX")
  expect_equal(fr(c("44 av. J.-C.", "44 avant J\u00e9sus-Christ")), rep("-0043", 2))
  expect_equal(fr("200 apr. J.-C."), "0200")
  expect_equal(fr("la 5e semaine de 2026"), "2026-01-26..2026-02-01")
  expect_equal(fr("le 103e jour de 2026"), "2026-04-13")
  expect_equal(fr(c("l'\u00e9t\u00e9 2026", "l\u2019hiver 2026", "printemps 2026")),
               c("2026-06..2026-08", "2026-12..2027-02", "2026-03..2026-05"))
  expect_equal(fr(c("d\u00e9but 1990", "mi-1990", "\u00e0 la fin de 1990")),
               c("1990-01..1990-04", "1990-05..1990-08", "1990-09..1990-12"))
  expect_equal(fr("le dernier jour de f\u00e9vrier 2020"), "2020-02-29")
})

test_that("French Roman calendar references and connectives parse", {
  expect_equal(fr("les ides de mars, 44 av. J.-C."), "-0043-03-15")
  expect_equal(fr("les calendes de janvier 2019"), "2019-01-01")
  expect_equal(fr(c("entre le 13 et le 15 f\u00e9vrier 1977",
                    "du 13 au 15 f\u00e9vrier 1977")),
               rep("1977-02-13..1977-02-15", 2))
  expect_equal(fr("le 13 ou le 15 f\u00e9vrier 1977"), "{1977-02-13,1977-02-15}")
})

test_that("French input leaves digits and Roman years alone", {
  expect_equal(fr(c("2019-03-15", "2019-03-15..2019-04-01", "MDCCLXXVI")),
               c("2019-03-15", "2019-03-15..2019-04-01", "1776"))
  expect_warning(out <- fr("pas une date"), "could not be parsed")
  expect_true(is.na(out))
})

test_that("a century before the common era keeps its era", {
  expect_equal(as.character(as_messydate("the 5th century BC")), "-04XX")
  expect_equal(as.character(as_messydate("1st century BC")), "-00XX")
  expect_equal(as.character(vmax(as_messydate("1st century BC"))), "0000-12-31")
  expect_equal(as.character(as_messydate("the 19th century")), "18XX")
})
