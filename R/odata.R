
#' Client class for interacting with OData (Open Data) protocol web services.
#'
#' @param base_url The base URL of the OData web service to make a client to.
#' @param ... Other arguments.
#'
#' @details
#'
#' @rdname odata
#' @keywords internal
odata <- function(base_url, ...) {
  new_odata(base_url, ...)
}

new_odata <- function(base_url, ..., class = NULL) {
  dots <- list(...)
  members <- c(list("base_url" = base_url),
               if (length(dots) == 0L) NULL else dots)
  validate_odata(structure(members, class = c(class, "odata")))
}


validate_odata <- function(x) {
  x
}


is.odata <- function(x) {
  inherits(x, "odata")
}

print.odata <- function(x) {
  print(x$base_url)
  invisible(x)
}


#' Client to Stats NZ Open data web service.
#'
#' @param subscription_key The subscription key for the Stats NZ
#' Open data service
#'
#' @details
#'
#' @rdname statsnz
statsnz <- function(subscription_key) {
  base_url <- "https://api.stats.govt.nz/opendata/v1/"
  headers <- list(
    "Ocp-Apim-Subscription-Key" = subscription_key
  )
  odata(base_url, headers = headers, class = "statsnz")
}




#' View catalogue of available datasets for the OData service
#'
#' @param client an object inheriting from `odata`
#'
#' @details
#'
#' @return
catalogue <- function(client) UseMethod("catalogue")

catalogue.statsnz <- function(client) {
  endpoint <- "data.json"
  url <- paste(client$base_url, endpoint, sep = "/")
  get_request(url, client$headers)
}


