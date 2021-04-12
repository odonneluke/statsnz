

get_request <- function(url, headers = NULL, query = NULL) {
  # TODO check that headers and query are named lists or character vectors
  stopifnot(is.null(headers) || length(names(headers)) == length(headers))
  stopifnot(is.null(query) || length(names(query)) == length(query))
  if (!is.null(headers))
    headers <- vapply(headers, identity, character(1L))
  if (!is.null(query))
    query <- vapply(query, identity, character(1L))

  r <- httr::GET(url, httr::add_headers(.headers = headers), query = query)

  if (httr::http_error(r)) {
    status_code <- httr::status_code(r)
    stop(glue::glue("GET {url} failed: {status_code}"))
  }
  if (!httr::has_content(r)) {
    msg <- httr::http_type()
    message(glue::glue("{msg[['messgage']]}"))
    return(invisible())
  }
  httr::content(r)
}
