#?--------------------------------------------------------------------
# SECTION 1   : COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS4370
# Assignment  : Final Project - Create an R Package
# URL         : https://usflearn.instructure.com/courses/1926966/grades/5384441
# Filename    : LIS4370-FinalProject.R
# Purpose     : Build an R package to analyze the iris dataset
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-05-05-1200-00
# Updated     : 2025-05-05-1200-00
# License     : MIT
# Description : This script builds the IrisTools package with functions
#             : to calculate means, summarize, and visualize iris data
#             : using S3 classes and methods
#             :

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

show_comments <- TRUE  # Set to FALSE to hide all instructional comments

# Set the base directory.
baseDir <- r"(E:\finalProject)"

# Ensure and set the working directory.
tryCatch({
  print(paste("Current working directory:", getwd()))
  if (!dir.exists(baseDir)) stop("Directory does not exist: ", baseDir)
  setwd(baseDir)
  print(paste("Working directory successfully set to:", baseDir))
}, error = function(e) {
  stop("Directory setup failed: ", e$message)
})
# Ensure Output Width for Terminal Display (Optional, Unix-based Systems)
tryCatch({
  options(width = 80) # Adjust width as needed
}, error = function(e) {
  print("Could not set terminal width.")
})

# --------------------------------------------------------------------
# SECTION 3: DEPENDENCIES & INSTALLATION
# --------------------------------------------------------------------

# Required Libraries (Modify as needed)
required_packages <- c("dplyr", "ggplot2")  # Add more as needed

# Check, Install, and Load Required Libraries
tryCatch({
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      if (!require(pkg, character.only = TRUE)) {
        stop("Failed to load package after installation: ", pkg)
      }
      cat("Successfully loaded:", pkg, "\n")
    } else {
      cat("Package already loaded:", pkg, "\n")
    }
  }
}, error = function(e) {
  stop("Library setup failed: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 4: DATA UTILITY FUNCTIONS
# --------------------------------------------------------------------

# Function to Calculate Mean Measurements by Species
calc_iris_means <- function(data = iris) {
  if (show_comments) {
    cat("Calculating mean measurements by species...\n")
  }
  tryCatch({
    means <- dplyr::group_by(data, Species) %>%
      dplyr::summarize(
        Mean_Sepal_Length = mean(Sepal.Length, na.rm = TRUE),
        Mean_Sepal_Width = mean(Sepal.Width, na.rm = TRUE),
        Mean_Petal_Length = mean(Petal.Length, na.rm = TRUE),
        Mean_Petal_Width = mean(Petal.Width, na.rm = TRUE)
      )
    return(means)
  }, error = function(e) {
    stop("Error calculating means: ", e$message)
  })
}

# Function to Plot Iris Measurements
plot_iris <- function(data = iris) {
  if (show_comments) {
    cat("Creating scatterplot of Sepal Length vs Petal Length...\n")
  }
  tryCatch({
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Sepal vs Petal Length by Species", x = "Sepal Length", y = "Petal Length")
    print(plot)
    invisible(plot)
  }, error = function(e) {
    stop("Error creating plot: ", e$message)
  })
}

# Function to Summarize Iris Measurements
summarize_iris <- function(data = iris) {
  if (show_comments) {
    cat("Summarizing iris measurements...\n")
  }
  tryCatch({
    summary_stats <- data.frame(
      Measurement = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      Min = c(min(data$Sepal.Length), min(data$Sepal.Width), min(data$Petal.Length), min(data$Petal.Width)),
      Max = c(max(data$Sepal.Length), max(data$Sepal.Width), max(data$Petal.Length), max(data$Petal.Width)),
      Mean = c(mean(data$Sepal.Length), mean(data$Sepal.Width), mean(data$Petal.Length), mean(data$Petal.Width))
    )
    class(summary_stats) <- "iris_summary"
    return(summary_stats)
  }, error = function(e) {
    stop("Error summarizing data: ", e$message)
  })
}

# S3 Print Method for iris_summary
print.iris_summary <- function(x, ...) {
  if (show_comments) {
    cat("Printing iris_summary object...\n")
  }
  tryCatch({
    cat("Summary of Iris Measurements:\n")
    print(as.data.frame(x))
  }, error = function(e) {
    stop("Error printing summary: ", e$message)
  })
}

# Check for Missing Values
check_missing_values <- function(data) {
  missing_count <- sum(is.na(data))
  if (show_comments) {
    if (missing_count > 0) {
      cat("Warning:", missing_count, "missing values found.\n")
    } else {
      cat("No missing values found in the dataset.\n")
    }
  }
  return(missing_count)
}

# Summarize Data Frame
summarize_data <- function(data) {
  if (show_comments) {
    cat("Displaying summary statistics and data dimensions...\n")
  }
  tryCatch({
    print(summary(data))
    cat("Dimensions:", dim(data)[1], "rows by", dim(data)[2], "columns\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error summarizing data: ", e$message)
  })
}

# Aggregate Data
aggregate_data <- function(data, group_col, value_col) {
  if (show_comments) {
    cat("Aggregating data by group column:", group_col, "\n")
  }
  tryCatch({
    aggregated_data <- data %>%
      dplyr::group_by(.data[[group_col]]) %>%
      dplyr::summarize(result = mean(.data[[value_col]], na.rm = TRUE))
    return(aggregated_data)
  }, error = function(e) {
    stop("Error aggregating data: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 5: DATA I/O HANDLERS
# --------------------------------------------------------------------

# Read a File Based on Format (CSV Only by Default)
read_data_file <- function(file_path, file_type = "csv") {
  tryCatch({
    if (file_type == "csv") {
      data <- read.csv(file_path)
    } else {
      stop("Unsupported file type:", file_type)
    }
    if (show_comments) {
      cat("Successfully read", file_type, "file from", file_path, "\n")
    }
    return(data)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })
}

# Load Sample Data for Testing
load_sample_data <- function() {
  if (show_comments) {
    cat("Creating sample data...\n")
  }
  data <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 20, 15)
  )
  return(data)
}

# --------------------------------------------------------------------
# SECTION 6: DATA PROCESSING WORKFLOWS
# --------------------------------------------------------------------

# General Pipeline: Clean, Transform, Validate
process_data <- function(data) {
  tryCatch({
    if (show_comments) cat("Processing pipeline started...\n")
    cleaned_data <- clean_data(data)
    transformed_data <- transform_data(cleaned_data)
    check_missing_values(transformed_data)
    if (show_comments) cat("Pipeline completed.\n")
    return(transformed_data)
  }, error = function(e) {
    stop("Error in processing pipeline: ", e$message)
  })
}

# Function to Clean Data
clean_data <- function(data) {
  if (show_comments) {
    cat("Cleaning data: removing duplicates, handling missing values...\n")
  }
  tryCatch({
    data <- unique(data)  # Remove duplicates
    data <- na.omit(data)  # Drop rows with missing values
    return(data)
  }, error = function(e) {
    stop("Error cleaning data: ", e$message)
  })
}

# Function to Transform Data
transform_data <- function(data) {
  if (show_comments) {
    cat("Applying transformations: filtering, mutating, summarizing...\n")
  }
  tryCatch({
    transformed_data <- data %>%
      dplyr::filter(Sepal.Length > 0) %>%
      dplyr::mutate(Sepal_Ratio = Sepal.Length / Sepal.Width) %>%
      dplyr::group_by(Species) %>%
      dplyr::summarize(Mean_Sepal_Ratio = mean(Sepal_Ratio, na.rm = TRUE))
    return(transformed_data)
  }, error = function(e) {
    stop("Error transforming data: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 7: DATABASE OPERATIONS (SQLite Placeholder)
# --------------------------------------------------------------------

# Connect to SQLite Database
connect_sqlite <- function(db_path) {
  if (show_comments) cat("Connecting to SQLite DB at:", db_path, "\n")
  db_connection <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  return(db_connection)
}

# CRUD Operations Placeholder
create_table <- function(db_connection, schema) { }
insert_record <- function(db_connection, table_name, record_data) { }
read_records <- function(db_connection, query) { }
update_record <- function(db_connection, table_name, condition, new_values) { }
delete_record <- function(db_connection, table_name, condition) { }

# --------------------------------------------------------------------
# SECTION 8: ANALYSIS FUNCTIONS
# --------------------------------------------------------------------

analyze_data <- function(data) {
  if (show_comments) cat("Analyzing data...\n")
  tryCatch({
    analysis_result <- data %>%
      dplyr::group_by(Species) %>%
      dplyr::summarize(Mean_Sepal_Length = mean(Sepal.Length, na.rm = TRUE))
    if (show_comments) cat("Analysis completed successfully.\n")
    return(analysis_result)
  }, error = function(e) {
    stop("Error during analysis: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 9: VISUALIZATION FUNCTIONS
# --------------------------------------------------------------------

visualize_data <- function(data) {
  if (show_comments) cat("Creating visualization...\n")
  tryCatch({
    plot_object <- ggplot2::ggplot(data, aes(x = Sepal.Length, y = Petal.Length)) +
      geom_point() +
      theme_minimal() +
      labs(
        title = "Sepal vs Petal Length",
        x = "Sepal Length",
        y = "Petal Length"
      )
    print(plot_object)
    if (show_comments) cat("Visualization completed successfully.\n")
    return(invisible(plot_object))
  }, error = function(e) {
    stop("Error creating visualization: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 10: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------

main <- function() {
  if (show_comments) cat("Starting script execution...\n")
  tryCatch({
    if (show_comments) cat("Step 1: Loading data...\n")
    raw_data <- iris
    summarize_data(raw_data)
    
    if (show_comments) cat("Step 2: Calculating means...\n")
    means_data <- calc_iris_means(raw_data)
    print(means_data)
    
    if (show_comments) cat("Step 3: Summarizing data...\n")
    summary_data <- summarize_iris(raw_data)
    print(summary_data)
    
    if (show_comments) cat("Step 4: Visualizing results...\n")
    plot_iris(raw_data)
    
    if (show_comments) cat("Script execution completed successfully.\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Script execution failed: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 11: VERSION HISTORY
# --------------------------------------------------------------------
# Version History:
# - Version 1.0 (2025-05-05-1200-00): Initial template.
# - Version 1.1 (2025-05-05-1200-00): Added IrisTools functions.
# - Version 1.2 (2025-05-05-1200-00): Integrated S3 class and methods.

# --------------------------------------------------------------------
# SECTION 12: ADDITIONAL NOTES
# --------------------------------------------------------------------

# Best Practices:
# - Ensure secure handling of API keys and credentials.
# - Keep code modular and organized for maintainability.
# - Validate data inputs to prevent unexpected errors.
# - Use consistent naming conventions for variables and functions.
# - Include appropriate documentation and comments.
# - Test functions with small datasets before full execution.

# --------------------------------------------------------------------
# END OF TEMPLATE
# --------------------------------------------------------------------