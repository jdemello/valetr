#' Searches for matches in series' labels
#'
#' Exploratory function. If the pattern successfuly matches one or more series labels, then a
#' \code{data.frame} is returned with the series label and link. The link can be used in the function \code{\link{getSeriesData}} to fetch
#' the data.
#'
#' @param pattern character string as regular expression. Only accepts character string. If length is greater than 1, the first element is used. Defaults to \code{NULL}.
#' The default option returns a \code{data.frame} with all series with labels and links in the API.
#' @param ... accepts the follwing extra arguments from \code{\link[base]{grep}}: \code{ignore.case}, \code{perl}, \code{fixed} and
#' \code{useBytes}.
#' @return An \eqn{n x 2} \code{data.frame} with columns representing labels and links respectively.
#' @seealso \code{\link{getSeriesInfo}}, \code{\link[base]{grep}}
#' @examples
#' # get a list of series that matches the pattern "CPI"
#'
#' cpiList <- regexSeriesLabel(pattern="cpi") # no match, returns an empty data.frame
#' cpiList2 <- regexSeriesLabel(pattern="cpi", ignore.case=TRUE) # finds match

regexSeriesLabel <- function(pattern=NULL,...){
  # initial argument validation ----
  stopifnot(inherits(pattern, "character") || inherits(pattern, "NULL"))

  # access elipsis ----
  dots <- list(...)

  # check if dots is empty
  if(length(dots) != 0){

    dotnames <- names(dots)

    # sift through args, keep valid ones
    validArgs <- c("ignore.case", "perl", "fixed", "useBytes")

    invalidInd <- which(!(dotnames %in% validArgs)) # find invalid args (if any)

    # if there is, then display warning msg and remove them from dots
    if(length(invalidInd) > 0){
      invalidArgsNames <- dotnames[invalidInd]
      msg <- paste0("Following argument(s) not valid in ...: ", paste0(invalidArgsNames, collapse = ", "))
      warning(msg, immediate. = TRUE)
      dots[invalidInd] <- NULL
    }

    # assign final dots with hierarchy
    dots <- dots[names(dots) %in% validArgs]
    if(length(dots)==0) dots <- NULL
  }

  # get series list ---

  # use 1st element only
  if(length(pattern) > 1) pattern <- pattern[[1]]

  # base url
  url <- "https://www.bankofcanada.ca/valet"

  # find series list
  series <- jsonlite::fromJSON(txt = paste0(url, "/lists/series"))

  # get label and link
  series <- do.call(rbind, lapply(series[["series"]], function(x) unlist(x)))

  # make it into data.frame
  series <- data.frame(series, stringsAsFactors = FALSE)
  rownames(series) <- NULL

  # subset list by pattern (if not NULL)
  if(is.null(pattern)) return(series)

  series <- series[do.call("grep", append(list(pattern=pattern, x=series[["label"]]), dots)), ]
  return(series)

  # return(series[grep(x=series[["label"]], pattern=pattern, ...), ])
}
