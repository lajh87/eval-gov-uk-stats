#' Scrape Gov.uk 
#' https://www.gov.uk/search/all?keywords=evaluation&content_purpose_supergroup%5B%5D=research_and_statistics&order=relevance
#' 7 Aug 23

base <- "https://www.gov.uk/"
pages <- 1:197

df <- purrr::map_df(cli::cli_progress_along(pages), function(i){
  
  q <- list(
    content_purpose_supergroup = "research_and_statistics",
    keywords = "evaluation",
    order = "relevance",
    page = i
  )
  req <- httr::GET(url = base, path = "search/all.atom", query = q)
  
  xml2::xml_find_all(xml2::read_xml(req), "//d1:entry") |>
    purrr::map(function(x){
      updated <-  xml2::xml_text(xml2::xml_child(x, 2))
      link <- xml2::xml_attr(xml2::xml_child(x,3), "href")
      title <- xml2::xml_text(xml2::xml_child(x,4))
      summary <- xml2::xml_text(xml2::xml_child(x,5))
      tibble::tibble(link, updated, title, summary)
    })  
})

df <- df |>
  dplyr::mutate(updated = as.Date(updated)) 
  dplyr::mutate(id = 1:dplyr::n()) 

df |> 
  dplyr::filter(updated >= "2020-01-01") |>
  dplyr::mutate(
    title = glue::glue("<a href = '{link}' target = '_blank'>{title}</a>")
    ) |>
  dplyr::select(-link) |>
  DT::datatable(escape = FALSE)


f <- readLines("data-raw/evaluation-filter.txt")

eval23 <- df[f,]
write.csv(eval23,"data/eval23.csv", row.names = FALSE )
write.csv(df, "data/all-search-results.csv", row.names = FALSE)
