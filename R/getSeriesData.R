#' Retrieves series' observations
#'
#' \code{valetr} workhorse function. \code{getSeriesData()} retrieves observations from one or
#' more series. This list may have one or more components. Each component is a series. This
#' function accepts query arguments as specified on the
#' \href{https://www.bankofcanada.ca/valet/docs#observations_by_series}{API's manual}.
#'
#' @param series character string of \code{length} \eqn{>= 1}. It accepts both series' link and
#' their name only (see \strong{Details}).
#'
#' @param ... additional API's query arguments. See \strong{Details}
#'
#' @return An object of class list of one or more \code{data.frame}s with \eqn{n x 5} dimensions each. The
#' colums are respectively: \code{series_name}, \code{series_label}, \code{series_desc},
#' \code{d} (observation date) and \code{v} (series values).
#'
#' @details
#'
#' \strong{\code{series}}
#'
#'     The argument \code{series} accepts a character vector in which each element is either
#'     a series link or a series name. \code{getSeriesData()} does not throw an error if some
#'     components do not have a valid name or link in the API. Instead, \code{getSeriesData()}
#'     issues a warning message with the series and API error message details. Additionally,
#'     the final output removes elements without a valid link. If all components in
#'     \code{series} do not have a valid address then the function throws an error.
#'
#' \strong{\code{...}}
#'
#'    The following arguments are accepted in \code{...}: \code{start_date}, \code{end_date},
#'    \code{recent}, \code{recent_weeks}, \code{recent_months}, \code{recent_years}. These are
#'    the API's options for time subsetting (see \href{https://www.bankofcanada.ca/valet/docs#observations_by_series}{API documentation}).
#'    Only \code{start_date} and \code{end_date} can be jointly used in \code{...}. Using any
#'    other combination of two or more arguments causes \code{getSeriesData()} to accept only
#'    one of them according to a pre-established hierarchy. This argument hierarchy causes the function
#'    to ignore the additional arguments. The hiearchy flows as follows:
#'    \itemize{
#'        \item{\code{start_date} and \code{end_date}}
#'        \item{\code{recent}}
#'        \item{\code{recent_weeks}}
#'        \item{\code{recent_months}}
#'        \item{\code{recent_years}}
#'    }
#'
#'    The function converts numeric inputs to \code{recent}.
#'
#'    The series observations may have a frequency that is not compatible to the query
#'    (i.e. recent_weeks query in a monthly series). Valet API converts the query time
#'    interval into the corresponding frequency of the series. If the time interval component
#'    is less than the most recent observation in the series, \code{getSeriesData()} returns
#'    a \code{data.frame} with a single row containing the series information. This behaviour
#'    allows \code{getSeriesData()} to return some series for which there is a valid interval.
#'    Also note that any invalid arguments passed to \code{...} is ignored during execution.
#'    For more details on the arguments' input format, see \strong{examples}, the package's
#'    vignette and \href{https://www.bankofcanada.ca/valet/docs#observations_by_series}{Valet's documentation}.
#'
#' @examples
#'
#' \donttest{
#'
#' ### step 1: find the series link or name
#' # get series info
#' seriesInfo <- getSeriesInfo(patternGroupLabel="(?i)consumer price",
#'                             patternSeriesLabel="(?i)seasonally")
#'
#' # use link from resulting object to get series obs
#' series <- getSeriesData(seriesInfo[["series_link"]])
#'
#' #### query example ####
#' # ex1: start and end date
#' series <- getSeriesData(seriesInfo[["series_link"]],
#'                         start_date="2010-01-01", end_date="2012-01-01")
#'
#' # ex2: the most recent 10 obs
#' series <- getSeriesData(seriesInfo[["series_link"]], recent=10L)
#'
#' # ex3: most recent weeks
#' series <- getSeriesData(seriesInfo[["series_link"]], recent_weeks=20L)
#'
#' # ex4: most recent months
#' series <- getSeriesData(seriesInfo[["series_link"]], recent_months=29L)
#'
#' # ex4: most recent years
#' series <- getSeriesData(seriesInfo[["series_link"]], recent_years=20L)
#' }

getSeriesData <- function(series, ...){

  # params requirements ----
  stopifnot(!missing(series), !inherits(series, "NULL"), inherits(series, "character"))

  # prepare series urls ----

  # assign prefix object - url for observations by series
  prefix <- "https://www.bankofcanada.ca/valet/series/"

  baseQuery <- "https://www.bankofcanada.ca/valet/observations/"

  # remove any NAs from series if necessary
  if(any(is.na(series))) series <- series[!is.na(series)]

  # add http... prefix to series name if necessary
  series <- lapply(series, function(x){if(startsWith(x, prefix)) return(gsub(x=x,pattern=prefix,replacement=baseQuery));return(paste0(baseQuery, x))})

  # access elipsis variables ----
  dots <- list(...)

  # check if dots is empty
  if(length(dots) != 0){

    dotnames <- names(dots)

    # sift through args, keep valid ones
    validArgs <- c("start_date", "end_date", "recent", "recent_weeks", "recent_months", "recent_years")

    invalidInd <- which(!(dotnames %in% validArgs)) # find invalid args (if any)

    # if there is, then display warning msg and remove them from dots
    if(length(invalidInd) > 0){
      invalidArgsNames <- dotnames[invalidInd]
      msg <- paste0("Following argument(s) not valid in ...: ", paste0(invalidArgsNames, collapse = ", "))
      warning(msg, immediate. = TRUE)
      dots[invalidInd] <- NULL
    }

    # assign final dots with hierarchy
    dots <- .dotsHierarchy(dots, dotnames, validArgs)

    # form query
    qry <- paste0("?",paste0(names(dots), "=",sapply(dots, function(x) x), collapse="&"))

    # append query to url(s)
    series <- lapply(series, function(x) paste0(x, qry))
  }

  # get series ----
  seriesObs <- lapply(series, function(x) tryCatch(jsonlite::fromJSON(x), error=function(e) identity(e)))

  # check if any links do not work
  errorInd <- which(sapply(seriesObs, inherits, c(what="error")), arr.ind = TRUE)
  urlError <- series[errorInd]

  if(length(urlError) != 0){
    if(length(urlError) == length(series)) stop("Page(s) unavailable. Check the series' name(s).")
    errorUrlMsg <- paste0(urlError," - error msg: ", sapply(seriesObs[errorInd], function(x) x[["message"]]))
    msg <- paste0("Following page(s) unavailable:\n\n", paste0(errorUrlMsg, collapse="\n\n"),
                  "\n\n",
                  "Removing the series above from the final output.")

    # display warnings
    warning(msg)

    # filter out object with error class
    seriesObs <- setdiff(seriesObs, seriesObs[errorInd])
  }

  # assemble output into dframe
  out <- lapply(seriesObs, function(x){
    # list component with observations
    obs <- x[["observations"]]
    if(length(obs)>0){
      # find date and value columns
      cols <- names(unlist(.drillDown(obs, "character"))) # vectors must be character, else drill down

      els <- strsplit(cols, "\\.") # list with vectors of drill down path

      dObs <- do.call(cbind, lapply(els, function(x) obs[[x]])) # n x m matrix -input to final frame

      colnames(dObs) <- cols

      d <- data.frame(series_name = names(x[["seriesDetail"]]),
                      series_label=unlist(x[["seriesDetail"]])[[1]],
                      series_desc=unlist(x[["seriesDetail"]])[[2]],
                      dObs, stringsAsFactors = FALSE)
    }else{
      d <- data.frame(series_name = names(x[["seriesDetail"]]),
                      series_label=unlist(x[["seriesDetail"]])[[1]],
                      series_desc=unlist(x[["seriesDetail"]])[[2]],
                      stringsAsFactors = FALSE)
    }
    return(d)
  })

  # name objects in lists with series name
  names(out) <- sapply(seriesObs, function(x) names(x[["seriesDetail"]]))

  return(out)
}
