## code to prepare `battles` dataset goes here

battles <- tibble::tribble(~Battle, ~Date,
                           "Operation MH-2","2001-03-08",
                           "Battle of Tetovo","2001-03-16..2001-08-13",
                           "Operation Vaksince","2001-05-25",
                           "Battle of Vedeno","2001-08-13..2001-08-26",
                           "Operation Crescent Wind","2001-10-07..2001-12-XX",
                           "Operation Rhino","2001-10-19..2001-10-20",
                           "Battle of Mazar-e-Sharif","2001-11-09",
                           "Siege of Kunduz","2001-11-11..2001-11-23",
                           "Battle of Herat","2001-11-12",
                           "Battle of Kabul","2001-11-13..2001-11-14",
                           "Battle of Tarin Kowt","2001-11-13..2001-11-14",
                           "Operation Trent","2001-11-15~..2001-11-30~",
                           "Battle of Kandahar","2001-11-22..2001-12-07",
                           "Battle of Qala-i-Jangi","2001-11-25..2001-12-01",
                           "Battle of Tora Bora","2001-12-12..2001-12-17",
                           "Battle of Shawali Kowt","2001-12-03",
                           "Battle of Sayyd Alma Kalay","2001-12-04",
                           "Battle of Amami-Oshima","2001-12-22",
                           "Tsotsin-Yurt operation","2001-12-30..2002-01-03")
battles$Date <- as_messydate(battles$Date)
battles

usethis::use_data(battles, overwrite = TRUE)
