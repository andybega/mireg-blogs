# Fix some ugly country names from countrycode package
prettyc <- function(country.names) {
  cn <- country.names
  dict <- matrix(ncol=2, byrow=TRUE, c(
    "Burkina Faso \\(Upper Volta\\)", "Burkina Faso",
    "Syrian Arab Republic", "Syria",
    "Congo, the Democratic Republic of the", "DR Congo",
    "Yemen Arab Republic", "Yemen",
    "Russian Federation", "Russia",
    "Bolivia, Plurinational State of", "Bolivia",
    "Venezuela, Bolivarian Republic of", "Venezuela",
    "Central African Republic", "CAR",
    "Macedonia, the former Yugoslav Republic of", "Macedonia",
    "Iran, Islamic Republic of", "Iran",
    "Moldova, Republic of", "Moldova",
    "Korea, Democratic People's Republic of", "North Korea",
    "Federal Republic of Germany", "Germany",
    "Lao People's Democratic Republic", "Laos",
    "Tanzania, United Republic of", "Tanzania",
    "Russia \\(Soviet Union\\)", "Russia",
    "Korea, People's Republic of", "Korea, North",
    "Myanmar \\(Burma\\)", "Burma",
    "Vietnam, Democratic Republic of", "Vietnam",
    "German Federal Republic", "Germany",
    "Macedonia \\(Former Yugoslav Republic of\\)", "Macedonia (FYR)",
    "Belarus \\(Byelorussia\\)", "Belarus",
    "Burkina Faso \\(Upper Volta\\)", "Burkina Faso",
    "Congo, Democratic Republic of \\(Zaire\\)", "Congo-Kinshasa",
    "Zimbabwe \\(Rhodesia\\)", "Zimbabwe",
    "Iran \\(Persia\\)", "Iran",
    "Turkey \\(Ottoman Empire\\)", "Turkey",
    "Yemen \\(Arab Republic of Yemen\\)", "Yemen",
    "Korea, Republic of", "Korea, South",
    "Sri Lanka \\(Ceylon\\)", "Sri Lanka",
    "Cambodia \\(Kampuchea\\)", "Cambodia",
    "Tanzania/Tanganyika", "Tanzania")
  )
  for (i in 1:nrow(dict)) {
    cn <- gsub(dict[i, 1], dict[i, 2], cn)
  }
  cn
}