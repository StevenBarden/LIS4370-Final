#' Calculate Mean Measurements by Species
#'
#' Computes the average of each measurement in the iris dataset by species.
#'
#' @param data A data frame, defaults to iris.
#' @return A data frame with mean measurements by species.
#' @export
calc_iris_means <- function(data = iris) {
  dplyr::group_by(data, Species) %>%
    dplyr::summarize(
      Mean_Sepal_Length = mean(Sepal.Length, na.rm = TRUE),
      Mean_Sepal_Width = mean(Sepal.Width, na.rm = TRUE),
      Mean_Petal_Length = mean(Petal.Length, na.rm = TRUE),
      Mean_Petal_Width = mean(Petal.Width, na.rm = TRUE)
    )
}

#' Plot Iris Measurements
#'
#' Creates a scatterplot of Sepal Length vs Petal Length, colored by species.
#'
#' @param data A data frame, defaults to iris.
#' @return A ggplot object (invisibly).
#' @export
plot_iris <- function(data = iris) {
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Sepal vs Petal Length by Species", x = "Sepal Length", y = "Petal Length")
  print(plot)
  invisible(plot)
}

#' Summarize Iris Measurements
#'
#' Creates a summary of min, max, and mean for each measurement.
#'
#' @param data A data frame, defaults to iris.
#' @return An object of class iris_summary with summary stats.
#' @export
summarize_iris <- function(data = iris) {
  summary_stats <- data.frame(
    Measurement = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    Min = c(min(data$Sepal.Length), min(data$Sepal.Width), min(data$Petal.Length), min(data$Petal.Width)),
    Max = c(max(data$Sepal.Length), max(data$Sepal.Width), max(data$Petal.Length), max(data$Petal.Width)),
    Mean = c(mean(data$Sepal.Length), mean(data$Sepal.Width), mean(data$Petal.Length), mean(data$Petal.Width))
  )
  class(summary_stats) <- "iris_summary"
  summary_stats
}

#' Print Method for iris_summary
#'
#' @param x An object of class iris_summary.
#' @param ... Additional arguments (ignored).
#' @export
print.iris_summary <- function(x, ...) {
  cat("Summary of Iris Measurements:\n")
  print(as.data.frame(x))
}
