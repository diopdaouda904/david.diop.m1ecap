#' Filtrer les jeux par genre
#'
#' @param df Un data.frame contenant une colonne Genres.
#' @param genre Un caractère, le genre à rechercher (ex: "RPG").
#' @return Un data.frame filtré.
#' @importFrom dplyr filter
#' @export
#' @examples
#' filtrer_genre(df_games, "RPG")
filtrer_genre <- function(df, genre) {
  if (!is.character(genre)) stop("genre doit être un caractère")
  dplyr::filter(df, grepl(genre, Genres, ignore.case = TRUE))
}

#' Calculer le rating moyen par genre
#'
#' @param df Un data.frame avec colonnes Genres et Rating.
#' @param stat La statistique : "mean", "max" ou "median".
#' @return Un data.frame avec le rating par genre.
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom tidyr unnest
#' @export
#' @examples
#' calcul_par_genre(df_games, stat = "mean")
calcul_par_genre <- function(df, stat = "mean") {
  fn <- switch(stat,
               mean   = mean,
               max    = max,
               median = median,
               stop("stat doit être 'mean', 'max' ou 'median'")
  )
  df |>
    dplyr::mutate(genre = strsplit(Genres, ",")) |>
    tidyr::unnest(genre) |>
    dplyr::mutate(genre = trimws(genre)) |>
    dplyr::group_by(genre) |>
    dplyr::summarise(
      rating_stat = fn(Rating, na.rm = TRUE),
      n_jeux = dplyr::n(),
      .groups = "drop"
    )
}

#' Visualiser le rating par genre
#'
#' @param df Un data.frame avec colonnes Genres et Rating.
#' @param stat La statistique à afficher : "mean", "max" ou "median".
#' @param n_genres Le nombre de genres à afficher (défaut : 10).
#' @return Un objet ggplot2.
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @importFrom dplyr slice_max
#' @export
#' @examples
#' plot_par_genre(df_games)
plot_par_genre <- function(df, stat = "mean", n_genres = 10) {
  donnees <- calcul_par_genre(df, stat = stat) |>
    dplyr::slice_max(order_by = rating_stat, n = n_genres)
  ggplot2::ggplot(donnees, ggplot2::aes(x = reorder(genre, rating_stat),
                                        y = rating_stat)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("Rating", stat, "par genre"),
      x = "Genre",
      y = paste("Rating (", stat, ")")
    ) +
    ggplot2::theme_minimal()
}
