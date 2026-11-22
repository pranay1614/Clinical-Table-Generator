rm(list=ls())
library(dplyr)
library(haven)
library(DT)


read_adam_dataset <- function(file_path, file_name = NULL) {
  
  if (!is.null(file_name)) {
    file_ext <- tolower(tools::file_ext(file_name))
  } else {
    file_ext <- tolower(tools::file_ext(file_path))
  }
  
  if (!file.exists(file_path)) {
    stop("Uploaded dataset file does not exist.")
  }
  
  if (file_ext == "csv") {
    
    data <- read.csv(
      file_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
  } else if (file_ext == "xpt") {
    
    data <- haven::read_xpt(file_path)
    
  } else if (file_ext == "sas7bdat") {
    
    data <- haven::read_sas(file_path)
    
  } else {
    
    stop("Unsupported file format. Upload CSV, XPT or SAS7BDAT.")
    
  }
  
  as.data.frame(data)
  
}

validate_adsl <- function(data,
                          usubjid = "USUBJID",
                          trt = "TRT01A") {
  
  required_vars <- c(usubjid, trt)
  
  missing_vars <- setdiff(
    required_vars,
    names(data)
  )
  
  if (length(missing_vars) > 0) {
    
    stop(
      paste(
        "Missing required variables:",
        paste(missing_vars, collapse = ", ")
      )
    )
    
  }
  
  if (any(is.na(data[[usubjid]]) |
          trimws(as.character(data[[usubjid]])) == "")) {
    
    stop("USUBJID contains missing or blank values.")
    
  }
  
  if (anyDuplicated(data[[usubjid]]) > 0) {
    
    stop("ADSL must contain one record per USUBJID.")
    
  }
  
  if (any(is.na(data[[trt]]))) {
    
    stop("Treatment variable contains missing values.")
    
  }
  
  return(TRUE)
  
}

format_number <- function(x, digits = 1) {
  
  if (is.na(x)) {
    return("NA")
  }
  
  formatC(
    x,
    format = "f",
    digits = digits
  )
  
}

create_demographics_table <- function(
    data,
    trt_var = "TRT01A",
    usubjid = "USUBJID",
    cat_vars = c("SEX", "RACE"),
    cont_vars = c("AGE", "HEIGHT", "WEIGHT")) {
  
  validate_adsl(
    data,
    usubjid,
    trt_var
  )
  
  selected_vars <- c(cat_vars, cont_vars)
  
  invalid_vars <- setdiff(
    selected_vars,
    names(data)
  )
  
  if (length(invalid_vars) > 0) {
    
    stop(
      paste(
        "Selected variables not found:",
        paste(invalid_vars, collapse = ", ")
      )
    )
    
  }
  
  if (anyDuplicated(selected_vars) > 0) {
    stop("A variable cannot be selected more than once.")
  }
  
  if (length(selected_vars) == 0) {
    stop("Select at least one demographic variable.")
  }
  
  treatments <- unique(as.character(data[[trt_var]]))
  
  treatment_n <- vapply(
    treatments,
    function(trt) {
      sum(as.character(data[[trt_var]]) == trt)
    },
    numeric(1)
  )
  
  treatment_names <- paste0(
    treatments,
    " (N=",
    treatment_n,
    ")"
  )
  
  results <- list()
  
  add_result <- function(parameter, statistic, values) {
    
    results[[length(results) + 1]] <<- data.frame(
      Parameter = parameter,
      Statistic = statistic,
      as.list(setNames(
        as.character(values),
        treatment_names
      )),
      check.names = FALSE
    )
    
  }
  
  for (var in cont_vars) {
    
    stats <- c(
      "n",
      "Mean (SD)",
      "Median",
      "Minimum",
      "Maximum"
    )
    
    for (stat in stats) {
      
      values <- vapply(
        treatments,
        function(trt) {
          
          x <- data[
            as.character(data[[trt_var]]) == trt,
            var
          ]
          
          x <- suppressWarnings(
            as.numeric(as.character(x))
          )
          
          x <- x[!is.na(x)]
          
          if (stat == "n") {
            
            as.character(length(x))
            
          } else if (stat == "Mean (SD)") {
            
            if (length(x) == 0) {
              "NA"
            } else {
              paste0(
                format_number(mean(x)),
                " (",
                format_number(
                  if (length(x) > 1) sd(x) else NA_real_
                ),
                ")"
              )
            }
            
          } else if (stat == "Median") {
            
            if (length(x) == 0) "NA" else
              format_number(median(x))
            
          } else if (stat == "Minimum") {
            
            if (length(x) == 0) "NA" else
              format_number(min(x))
            
          } else {
            
            if (length(x) == 0) "NA" else
              format_number(max(x))
            
          }
          
        },
        character(1)
      )
      
      add_result(
        var,
        stat,
        values
      )
      
    }
    
  }
  
  for (var in cat_vars) {
    
    all_levels <- unique(
      as.character(data[[var]][!is.na(data[[var]])])
    )
    
    all_levels <- sort(all_levels)
    
    all_levels <- c(all_levels, "Missing")
    
    for (level in all_levels) {
      
      values <- vapply(
        treatments,
        function(trt) {
          
          subset <- data[
            as.character(data[[trt_var]]) == trt,
            ,
            drop = FALSE
          ]
          
          if (level == "Missing") {
            
            n <- sum(is.na(subset[[var]]))
            
          } else {
            
            n <- sum(
              !is.na(subset[[var]]) &
                as.character(subset[[var]]) == level
            )
            
          }
          
          denominator <- sum(
            as.character(data[[trt_var]]) == trt
          )
          
          percentage <- 100 * n / denominator
          
          paste0(
            n,
            " (",
            format_number(percentage),
            "%)"
          )
          
        },
        character(1)
      )
      
      add_result(
        var,
        level,
        values
      )
      
    }
    
  }
  
  final_table <- dplyr::bind_rows(results)
  
  return(final_table)
  
}
