library(testthat)

df_test <- data.frame(
  Title    = c("Game A", "Game B", "Game C", "Game D"),
  Genres   = c("RPG, Action", "Sport", "RPG", "Action"),
  Rating   = c(4.5, 3.0, 4.8, 2.5),
  Plays    = c(1000L, 500L, 2000L, 300L),
  Playing  = c(10L, 5L, 20L, 3L),
  Backlogs = c(50L, 30L, 100L, 10L),
  Wishlist = c(200L, 100L, 300L, 50L),
  `Times Listed`      = c(100L, 50L, 200L, 30L),
  `Number of Reviews` = c(80L, 40L, 160L, 20L),
  `Release Date`      = as.Date(c("2020-01-01","2019-06-15","2021-03-10","2018-11-01")),
  check.names = FALSE
)

test_that("filtrer_genre retourne un data.frame", {
  res <- filtrer_genre(df_test, "RPG")
  expect_s3_class(res, "data.frame")
})

test_that("filtrer_genre filtre correctement", {
  res <- filtrer_genre(df_test, "RPG")
  expect_equal(nrow(res), 2)
})

test_that("filtrer_genre erreur si genre non character", {
  expect_error(filtrer_genre(df_test, 123))
})

test_that("calcul_par_genre retourne un data.frame", {
  res <- calcul_par_genre(df_test)
  expect_s3_class(res, "data.frame")
})

test_that("calcul_par_genre a les bonnes colonnes", {
  res <- calcul_par_genre(df_test)
  expect_true("genre" %in% names(res))
  expect_true("rating_stat" %in% names(res))
})

test_that("plot_par_genre retourne un ggplot", {
  res <- plot_par_genre(df_test)
  expect_s3_class(res, "ggplot")
})
