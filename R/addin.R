
url_to_list <- function(url) {
  query <- httr::parse_url(url)$query
  no_query <- c("offset", "limit", "facets", "format")
  query <- query[!(names(query) %in% no_query)]

  max_n <- max(vapply(names(query), nchar, 0))


  elements <- lapply(seq_along(query), function(i) {
    paste0("  ",
           formatC(names(query)[i], width = -max_n, flag = " "),
           " = \"",
           query[[i]], "\"")
  })

    return(paste0("query <- list(\n",
           paste0(unlist(elements), collapse = ",\n"),
           "\n)"))
}

url_to_list_Addin <- function() {
  context   <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection[[1]]$text
  request  <- url_to_list(selection)
  location <- context$selection[[1]]$range
  rstudioapi::modifyRange(location = location,
                          text = request,
                          id = context$id)
}

