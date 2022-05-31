## code to prepare `battles` dataset
battles <- tibble::tribble(~Battle, ~Date,
                           "Operation MH-2", "2001 March 8",
                           "2001 Bangladesh–India border clashes", "2001-04-16..2001-04-20",
                           "Operation Vaksince", "25-5-2001",
                           "Alkhan-Kala operation", "2001-6-22..2001-6-28",
                           "Battle of Vedeno", "2001-8-13..2001-8-26",
                           "Operation Crescent Wind", "2001-10-7..2001-12?",
                           "Operation Rhino", "2001-10-19..2001-10-20",
                           "Battle of Mazar-e-Sharif","2001-11-9",
                           "Siege of Kunduz", "2001-11-11..2001-11-23",
                           "Battle of Herat", "Twelve of November of two thousand and twenty-one",
                           "Battle of Kabul", "2001-11-13..2001-11-14",
                           "Battle of Tarin Kowt", "2001-11-13:2001-11-14",
                           "Operation Trent", "2001-11-15~..2001-11-30~",
                           "Battle of Kandahar", "2001-11-22..2001-12-07",
                           "Battle of Qala-i-Jangi", "2001-11-25:2001-12-01",
                           "Battle of Tora Bora", "2001-12-12..2001-12-17",
                           "Battle of Shawali Kowt", "2001-12-3",
                           "Battle of Sayyd Alma Kalay", "2001-12-4",
                           "Battle of Amami-Oshima", "2001-12-22",
                           "Tsotsin-Yurt operation", "2001-12-30:2002-01-03")
battles$Date <- as_messydate(battles$Date)
battles
usethis::use_data(battles, overwrite = TRUE)
