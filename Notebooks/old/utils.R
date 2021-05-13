first_non_zero <- function(x) {
  out <- 1L
  if (all(x == 0, na.rm = TRUE)) {
    out <- length(x)
  } else if (any(x == 0, na.rm = TRUE)) {
    out <- which(x != 0)[1]
  }
  out
  # min(which(x != 0))
}

intervals <- function(x, value = 0) {
  y <- rle(x)
  out <- y$lengths[y$values == value]
  if (length(out) == 0) {
    out <- 1
  }
  out
}

demand <- function(x) {
  x[x != 0]
}

ts_type <- function(ADI, CV2) {
  if (ADI > (4 / 3)) {
    if (CV2 > 0.5) {
      Type <- "Lumpy"
    } else{
      Type <- "Intermittent"
    }
  } else{
    if (CV2 > 0.5) {
      Type <- "Erratic"
    } else{
      Type <- "Smooth"
    }
  }
  Type
}

statistics <- function(x) {
  
  # Clean input data removing NAN's and initial zeros
  x <- x[!is.na(x)]
  fnz <- first_non_zero(x)
  x <- x[seq(fnz, length(x) )]
  D <- demand(x)
  
  tibble::tibble(
    len    = length(x),
    ADI    = mean(intervals(x)),
    CV2    = (sd(D) / mean(D)) ^ 2,
    Type   = ts_type(ADI, CV2),
    Min    = min(x),
    Low25  = as.numeric(quantile(x, 0.25)),
    Mean   = mean(x),
    Median = median(x),
    Up25   = as.numeric(quantile(x, 0.75)),
    Max    = max(x),
    pz     = length(x[x == 0]) / len,
    fnz    = fnz
  )
}


# https://purrple.cat/blog/2018/03/02/multiple-lags-with-tidy-evaluation/
add_lags <- function(.data, var, lags = seq(3), prefix = "lag_") {
  var <- enquo(var)
  expr <- map(lags, ~ quo(lag(!!var, !!.x))) %>%
    set_names(sprintf("%s%s_%02d", prefix, rlang::quo_text(var), lags))
  
  mutate(.data, !!!expr )
}




time_split <- function(.data, last_train, h = 12) {
  
  if (!inherits(last_train, "Date")) {
    last_train <- as.Date(last_train)
  }
  
  train <- .data %>% 
    filter(date <= last_train)
  
  test <- .data %>% 
    filter(date > last_train & date <= last_train + months(h))
  
  list(training = train, testing = test)
}