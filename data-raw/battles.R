## code to prepare `battles` dataset
battles <- tibble::tribble(~Battle, ~Date, ~Parties,
                           "Operation MH-2", "2001 March 8", "MK-National Libration Army(MK)",
                           "2001 Bangladesh–India border clashes", "2001-04-16..2001-04-20", "BD-ID",
                           "Operation Vaksince", "25-5-2001", "MK-National Libration Army(MK)",
                           "Alkhan-Kala operation", "2001-6-22..2001-6-28", "RU-Chechen Republic",
                           "Battle of Vedeno", "2001-8-13..2001-8-26", "RU-Chechen Insurgents",
                           "Operation Crescent Wind", "2001-10-7..2001-12?", "US/UK-Taliban",
                           "Operation Rhino", "2001-10-19..2001-10-20", "US-Taliban",
                           "Battle of Mazar-e-Sharif","2001-11-9", "US/Northern Alliance-Taliban/al-Qaeda",
                           "Siege of Kunduz", "2001-11-11..2001-11-23", "US/Northern Alliance-Taliban/al-Qaeda",
                           "Battle of Herat", "Twelve of November of two thousand and one", "US/Northern Alliance/Iran-Taliban",
                           "Battle of Kabul", "2001-11-13..2001-11-14", "US/Northern Alliance-Taliban",
                           "Battle of Tarin Kowt", "2001-11-13:2001-11-14", "US/Eastern Alliance-Taliban",
                           "Operation Trent", "2001-11-~15..2001-11-~30", "US/UK-Taliban/al-Qaeda",
                           "Battle of Kandahar", "2001-11-22..2001-12-07", "US/AU/Eastern Alliance-Taliban",
                           "Battle of Qala-i-Jangi", "2001-11-25:2001-12-01", "US/UK/Northern Alliance-Taliban/al-Qaeda",
                           "Battle of Tora Bora", "2001-12-12..2001-12-17", "US/Northern Alliance-Taliban/al-Qaeda",
                           "Battle of Shawali Kowt", "2001-12-3", "US/Eastern Alliance-Taliban",
                           "Battle of Sayyd Alma Kalay", "2001-12-4", "US/Eastern Alliance-Taliban",
                           "Battle of Amami-Oshima", "2001-12-22", "JP-KP",
                           "Tsotsin-Yurt operation", "2001-12-30:2002-01-03", "RU-Chechen Insurgents") %>%
  mutate(US_party = ifelse(grepl("US", Parties), 1, 0),
         n_actors = c(2, 2, 2, 2, 2, 3, 2, 4, 4, 4, 3, 3, 4, 4, 5, 4, 3, 3, 2, 2))
battles$Date <- messydates::as_messydate(battles$Date)
battles
usethis::use_data(battles, overwrite = TRUE)
