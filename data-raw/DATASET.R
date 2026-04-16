## code to prepare `DATASET` dataset goes here

# Source : https://www.kaggle.com/datasets/arnabchaki/popular-video-games-1980-2023
url_jeu_de_donnees <- "https://www.kaggle.com/datasets/arnabchaki/popular-video-games-1980-2023"

library(readr)
library(dplyr)
library(lubridate)

# Fonction conversion K
convertir_k <- function(x) {
  x <- trimws(as.character(x))
  case_when(
    grepl("K", x) ~ as.numeric(gsub("K", "", x)) * 1000,
    TRUE ~ as.numeric(x)
  )
}

# Chargement du fichier brut
df <- read_delim("data-raw/games.csv") |> select(-1)

# Nettoyage
df_games <- df |>
  select(-Summary, -Reviews, -Team) |>
  mutate(
    `Release Date`      = mdy(`Release Date`),
    Plays               = convertir_k(Plays),
    Playing             = convertir_k(Playing),
    Backlogs            = convertir_k(Backlogs),
    Wishlist            = convertir_k(Wishlist),
    `Times Listed`      = convertir_k(`Times Listed`),
    `Number of Reviews` = convertir_k(`Number of Reviews`)
  ) |>
  filter(`Release Date` >= "2000-01-01") |>
  distinct(Title, .keep_all = TRUE)

usethis::use_data(df_games, overwrite = TRUE)
