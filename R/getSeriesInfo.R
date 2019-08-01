#' Retrieves series information
#'
#' Exploratory function (albeit more complete than \code{\link{regexSeriesLabel}}). The pattern arguments searches for information on
#' series based on their group label and series label. A \code{data.frame} is returned with the series' group name, group label, group
#' description, series label, series link and series name. This function utilizes the "Series Group" route in the Valet API and provides
#' the full set of information available to a series. The series' link (or name) can be used in the function \code{\link{getSeriesData}}
#' to fecth data.
#'
#' @param patternGroupLabel character string as regular expression. Only accepts character string or \code{NULL} (default).
#' If length is greater than 1, the first element is used. \code{patternGroupLabel=NULL} retrieves the details for all groups in the
#' API.
#' @param patternSeriesLabel character string as regular expression. Only accepts character string or \code{NULL} (default).
#' If length is greater than 1, the first element is used. If \code{NULL}, no pattern-matching is executed on the series' label.
#' @param ... accepts the follwing extra arguments from \code{\link[base]{grep}}: \code{ignore.case}, \code{perl}, \code{fixed} and
#' \code{useBytes}.
#' @return An \eqn{n x 6} \code{data.frame} with columns representing the different characteristic of a series.
#' @details The \code{data.frame} returned from this function contains all information available in the
#' \href{https://www.bankofcanada.ca/valet/docs#series_groups}{Series Group} router in the Valet API. This function is more complete than
#' \code{regexSeriesLabel}. \code{getSeriesInfo} returns not only the series label and link (also available in \code{regexSeriesLabel}) but
#' also a series' group name, group label, group description and series name.
#'
#'     If \code{patternGroupLabel} is \code{NULL}, then the function will retrieve information for all groups (and their associated series).
#'     This results in a longer processing time. A progress bar tracks the retrieval process.
#' @seealso \code{\link{getSeriesInfo}}, \code{\link[base]{grep}}
#' @examples
#'
#' # returns series info
#' seriesInfo <- getSeriesInfo(patternGroupLabel="(?i)consumer price",
#'                             patternSeriesLabel="(?i)seasonally")

getSeriesInfo <- function(patternGroupLabel=NULL,patternSeriesLabel=NULL,...){

  # params requirements
  stopifnot(inherits(patternGroupLabel, "character") || inherits(patternGroupLabel, "NULL"))

  stopifnot(inherits(patternSeriesLabel, "character") || inherits(patternSeriesLabel,"NULL"))

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

  # get groups desc
  groups <- jsonlite::fromJSON("https://www.bankofcanada.ca/valet/lists/groups")

  # use 1st element in patternGroupLabel if necessary
  if(length(patternGroupLabel) > 1) patternGroupLabel <- patternGroupLabel[[1]]

  # use 1st element in patternSeriesLabel if necessary
  if(length(patternSeriesLabel) > 1) patternSeriesLabel <- patternSeriesLabel[[1]]

  # if null pattern, get all groups' desc, series and links
  if(is.null(patternGroupLabel)){

    groups <- do.call(rbind, lapply(groups$groups, function(x) x$link))

    pb <- utils::txtProgressBar(min = 0, max = 100, initial = 0)
    seriesInfo <- vector(mode="list", length=length(groups))
    groupsLen <- length(groups)
    for(i in seq_along(groups)){
      seriesInfo[[i]] <- jsonlite::fromJSON(groups[[i]])
      cat(sprintf("\rRetrieving series labels and names. This process may take a few minutes...%.2f%% completed...", (i/groupsLen) * 100))
      if(i == groupsLen) cat("\n")
    }
    close(pb)

    m <- do.call(rbind, lapply(seriesInfo, function(x){
      len <- length(x[["groupDetails"]]$groupSeries)
      m <- matrix(data=NA, nrow=len, ncol=6)
      m[, 1] <- x[["groupDetails"]]$name
      m[, 2] <- x[["groupDetails"]]$label
      m[, 3] <- x[["groupDetails"]]$description
      m[, 4] <- sapply(x[["groupDetails"]][["groupSeries"]], `[[`, 1)
      m[, 5] <- sapply(x[["groupDetails"]][["groupSeries"]], `[[`, 2)
      m[, 6] <- names(x[["groupDetails"]][["groupSeries"]])
      return(m)
      # c(x[["groupDetails"]]$name,x[["groupDetails"]]$label, x[["groupDetails"]]$description)
    }))

    out <- data.frame(m, stringsAsFactors = FALSE)
    names(out) <- c("group_name", "group_label", "group_desc", "series_label", "series_link", "series_name")
    rownames(out) <- NULL
    return(out)
  }

  # search pattern in group
  groups <- .searchGroupLabel(groups=groups,pattern=patternGroupLabel,dots=dots)

  # control
  if(length(groups)==0) stop("No group label matching 'pattern'.")

  # into matrix with links
  groups <- do.call(rbind, lapply(groups, function(x) x$link))

  seriesInfo <- vector(mode="list", length=length(groups))
  groupsLen <- length(groups)
  for(i in seq_along(groups)){
    seriesInfo[[i]] <- jsonlite::fromJSON(groups[[i]])
    cat(sprintf("\rRetrieving series labels and names. This process may take a few minutes...%.2f%% completed...", (i/groupsLen) * 100))
    if(i == groupsLen) cat("\n")
  }

  m <- do.call(rbind, lapply(seriesInfo, function(x){
    len <- length(x[["groupDetails"]]$groupSeries)
    m <- matrix(data=NA, nrow=len, ncol=6)
    m[, 1] <- x[["groupDetails"]]$name
    m[, 2] <- x[["groupDetails"]]$label
    m[, 3] <- x[["groupDetails"]]$description
    m[, 4] <- sapply(x[["groupDetails"]][["groupSeries"]], `[[`, 1)
    m[, 5] <- sapply(x[["groupDetails"]][["groupSeries"]], `[[`, 2)
    m[, 6] <- names(x[["groupDetails"]][["groupSeries"]])
    return(m)
    # c(x[["groupDetails"]]$name,x[["groupDetails"]]$label, x[["groupDetails"]]$description)
  }))

  out <- data.frame(m, stringsAsFactors = FALSE)
  names(out) <- c("group_name", "group_label", "group_desc", "series_label", "series_link", "series_name")
  rownames(out) <- NULL

  # return dframe without label filter in patternSeriesLabel
  if(is.null(patternSeriesLabel)) return(out)

  # subset out and return df
  # grepl(x=out[["series_label"]], pattern=patternSeriesLabel, ...)
  # out[grepl(x=out[["series_label"]], pattern=patternSeriesLabel, ...), ]
  out <- out[do.call("grep", append(list(pattern=patternSeriesLabel,x=out[["series_label"]]), dots)), ]
  rownames(out) <- NULL
  return(out)
}
