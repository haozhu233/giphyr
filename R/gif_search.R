#' Search GIFs by key words throught giphy API
#'
#' @param query search query term or phrase.
#' @param limit (optional) number of results to return, maximum 100. Default 10.
#' @param offset (optional) results offset, defaults to 0.
#' @param rating limit results to those rated (y,g, pg, pg-13 or r). Default g.
#' @param img_format A vector of strings deciding what formats of images you
#' want to receive. Possible options include "fixed_height",
#' "fixed_height_still", "fixed_height_downsampled", "fixed_width",
#' "fixed_width_still", "fixed_width_downsampled", "fixed_height_small",
#' "fixed_height_small_still", "fixed_width_small", "fixed_width_small_still",
#' "downsized", "downsized_still", "downsized_large", "downsized_medium",
#' "original", "original_still", "preview_gif", "fixed_height_mp4",
#' "fixed_width_mp4", "fixed_height_small_mp4", "fixed_width_small_mp4",
#' "original_mp4", "looping_mp4", "preview_mp4" & "downsized_small_mp4". An Image
#' with fixed height or width has a fixed length of the edge at 200px. A small
#' fixed height or width images has a 100px-long fixed edge. "Downsampled"
#' images are compressed and are good for preview. "Still" images is not a GIF
#' and will not be animated.
#'
#' @examples
#' gif_search("cat")
#' gif_search("dog", img_format = "downsized")
#' gif_search("panda", limit = 100, img_format = "downsized_small_mp4")
#'
#' @export
gif_search <- function(query, limit = 10, offset = 0, rating = "g",
                       img_format = c(
                         "fixed_height", "fixed_height_still",
                         "fixed_height_downsampled", "fixed_width",
                         "fixed_width_still", "fixed_width_downsampled",
                         "fixed_height_small", "fixed_height_small_still",
                         "fixed_width_small", "fixed_width_small_still",
                         "downsized", "downsized_still",
                         "downsized_large", "downsized_medium",
                         "original", "original_still",
                         "preview_gif", "fixed_height_mp4",
                         "fixed_width_mp4", "fixed_height_small_mp4",
                         "fixed_width_small_mp4", "original_mp4",
                         "looping_mp4", "preview_mp4",
                         "downsized_small_mp4"
                       )) {

  img_format <- match.arg(
    img_format,
    c(
      "fixed_height", "fixed_height_still",
      "fixed_height_downsampled", "fixed_width",
      "fixed_width_still", "fixed_width_downsampled",
      "fixed_height_small", "fixed_height_small_still",
      "fixed_width_small", "fixed_width_small_still",
      "downsized", "downsized_still",
      "downsized_large", "downsized_medium",
      "original", "original_still",
      "preview_gif", "fixed_height_mp4",
      "fixed_width_mp4", "fixed_height_small_mp4",
      "fixed_width_small_mp4", "original_mp4",
      "looping_mp4", "preview_mp4",
      "downsized_small_mp4"
    ), several.ok = TRUE
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

  out <- content_exporter(search_content)

  out_variables <- intersect(names(out), img_format)
  out_variables <- c(
    names(out)[1:(which(names(out) == "fixed_height") - 1)],
    out_variables
    )
  out <- out[out_variables]

  attr(out, "total_count") <- search_content$pagination$total_count
  attr(out, "count") <- search_content$pagination$count
  attr(out, "offset") <- search_content$pagination$offset


  return(out)
}

.giphy_api_key <- function()"GAUeYalixQovJJGRACGEaKRpNunOoH1q"

.giphy_url <- function()"http://api.giphy.com/"

search_query_parser <- function(x) {
  return(gsub("\\s+", "+", x))
}

content_exporter <- function(x) {
  non_scalar <- map_df(x$data, ~map(.x, length))
  non_scalar <- unlist(map(non_scalar, ~mean(.x, na.rm = T) == 1))
  non_scalar <- non_scalar[!non_scalar]
  non_scalar <- names(non_scalar)
  map_df(x$data, ~c(
    .x[which(!names(.x) %in% non_scalar)],
    gif_url(.x$images),
    mp4_url(.x$images))
  )
}

gif_url <- function(images) {
  images <- images[which(names(images) %in% c(
    "fixed_height", "fixed_height_still", "fixed_height_downsampled",
    "fixed_width", "fixed_width_still", "fixed_width_downsampled",
    "fixed_height_small", "fixed_height_small_still", "fixed_width_small",
    "fixed_width_small_still", "downsized", "downsized_still",
    "downsized_large", "downsized_medium", "original",
    "original_still","preview_gif"
  ))]
  out <- lapply(images, function(x) {
    if ("url" %in% names(x)) {
      x[[which(names(x) == "url")]]
    }
  })
  return(out)
}

mp4_url <- function(images) {
  mp4_variables <- c(
    "fixed_height", "fixed_width", "fixed_height_small", "fixed_width_small",
    "original", "looping", "preview", "downsized_small"
  )
  images <- images[which(names(images) %in% mp4_variables)]
  out <- lapply(images, function(x) {
    if ("mp4" %in% names(x)) {
      x[[which(names(x) == "mp4")]]
    }
  })
  names(out) <- paste0(mp4_variables, "_mp4")
  return(out)
}


