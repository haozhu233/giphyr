#' Search GIFs by key words throught giphy API
#'
#' @param query search query term or phrase.
#' @param limit (optional) number of results to return, maximum 100. Default 10.
#' @param offset (optional) results offset, defaults to 0.
#' @param rating limit results to those rated (y,g, pg, pg-13 or r). Default g.
#' @param img_format A vector of strings deciding what formats of images you
#' want to receive. Possible options are "fixed_height", "fixed_height_still",
#' "fixed_height_downsampled", "fixed_width", "fixed_width_still",
#' "fixed_width_downsampled", "fixed_height_small", "fixed_height_small_still",
#' "fixed_width_small", "fixed_width_small_still", "downsized",
#' "downsized_medium", "downsized_still", "downsized_large",
#' "original" and "original_still". An Image
#' with fixed height or width has a fixed length of the edge at 200px. A small
#' fixed height or width images has a 100px-long fixed edge. "Downsampled"
#' images are compressed and are good for preview. "Still" images is not a GIF
#' and will not be animated.
#'
#' @export
gif_search <- function(query, limit = 10, offset = 0, rating = "g",
                       img_format = c(
                         "fixed_height", "fixed_height_still",
                         "fixed_height_downsampled", "fixed_width",
                         "fixed_width_still", "fixed_width_downsampled",
                         "fixed_height_small", "fixed_height_small_still",
                         "fixed_width_small", "fixed_width_small_still",
                         "downsized_medium",
                         "downsized", "downsized_still", "downsized_large",
                         "original", "original_still")
                      ){

  img_format <- match.arg(
    img_format,
    c("fixed_height", "fixed_height_still",
      "fixed_height_downsampled", "fixed_width",
      "fixed_width_still", "fixed_width_downsampled",
      "fixed_height_small", "fixed_height_small_still",
      "fixed_width_small", "fixed_width_small_still",
      "downsized_medium",
      "downsized", "downsized_still", "downsized_large",
      "original", "original_still"), several.ok = TRUE
    )

  search_api_path <- "/v1/gifs/search"

  search_raw <- GET(
    url = .giphy_url(),
    path = search_api_path,
    query = list(
      q = search_query_parser(query),
      limit = limit,
      offset = offset,
      rating = rating,
      api_key = .giphy_api_key()
    ))

  if (search_raw$status_code != 200) {
    stop("HTML status code ", search_raw$status_code, " received. ",
         "Please wait for a while and retry.")
  }

  search_content <- content(search_raw)

  if (search_content$pagination$total_count == 0) {
    warning("Search returns no results. Please try again")
    return()
  }

  if (search_content$pagination$total_count == 1) {
    out <- content_exporter_for_one(search_content, img_format)
  }else{
    out <- content_exporter(search_content, img_format)
  }

  attr(out, "total_count") <- search_content$pagination$total_count
  attr(out, "count") <- search_content$pagination$count
  attr(out, "offset") <- search_content$pagination$offset


  return(out)
}

.giphy_api_key <- function()"dc6zaTOxFJmzC"

.giphy_url <- function()"http://api.giphy.com/"

search_query_parser <- function(x) {
  return(gsub("\\s+", "+", x))
}

content_exporter_1_ <- function(x, item) {
  x[item][[1]]
}
content_exporter_1 <- function(item, x) {
  unlist(lapply(x$data, content_exporter_1_, item))
}

content_exporter_2_ <- function(x, item) {
  x$image[item][[1]]$url
}
content_exporter_2 <- function(item, x) {
  unlist(lapply(x$data, content_exporter_2_, item))
}

content_exporter <- function(x, item) {
  cbind(
    as_tibble(sapply(c("id", "slug", "url", "source", "rating"),
                         content_exporter_1, x)),
    as_tibble(sapply(item,
                         content_exporter_2, x))
  )
}

content_exporter_for_one <- function(x, item) {
  out <- as_tibble(t(data.frame(c(
    sapply(c("id", "slug", "url", "source", "rating"), content_exporter_1, x),
    sapply(item, content_exporter_2, x)))))
  row.names(out) <- NULL
  return(out)
}

