# Roman population size estimates
roman_population <- tibble::tibble(period = c("200 BC:100 BC", "100 BC:1 AD", "1 AD:150 AD", "150 AD:200 AD"),
                                   population_min = c(3900000, 4400000, 5500000, 6000000),
                                   population_max = c(8000000, 10000000, 16000000, 2000000))
usethis::use_data(roman_population, overwrite = TRUE)
