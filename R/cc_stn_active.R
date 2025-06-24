#' Return active CIMIS Stations
#'
#' Return active CIMIS Stations
#'
#' @param data_dir The cache directory
#' @param crs The EPSG ID for the returned simple feature point layer
#' @param overwrite Overwrite the saved active stations file
#'
#' @details
#' This will download the active CIMIS stations and return them as a sf object . The default value for \code{crs} = 3310 is California Teale Albers (NAD83).
#'
#' As a side effect, the function can also save the active stations as a geojson file.
#'
#' @return A sf point layer containing the active CIMIS stations
#'
#' @importFrom httr2 request resp_body_json req_perform resp_status
#' @importFrom purrr map_chr
#' @importFrom conflicted conflicts_prefer
#' @import dplyr
#' @importFrom sf st_read st_as_sf st_transform st_write
#' @import crayon
#' @importFrom stringr str_extract
#' @export

cc_stn_active <- function(crs = 3310, use_cache = TRUE, data_dir = Sys.getenv("CC_DATADIR"), overwrite = FALSE) {

  conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)

  download_stations <- TRUE

  if (use_cache) {
    if(!dir.exists(data_dir)) stop(paste0("Can't find data directory"))
    stn_geojson <- file.path(data_dir, "cimis_stn.geojson")
    if (file.exists(stn_geojson) && !overwrite) {
      message(crayon::green(" - loading stored copy of cimis stations"))
      stn_sf <- sf::st_read(stn_geojson, quiet=TRUE)
      download_stations <- FALSE
    }
  }

  if (download_stations) {
    message(crayon::green(" - downloading active cimis stations"))

    stn_resp <- req_perform(request("https://et.water.ca.gov/api/station"))

    if (resp_status(stn_resp) != 200) stop("CIMIS API error")

    stn_lst <- stn_resp |> resp_body_json()

    stn_tbl <- tibble(
      stid = as.integer(purrr::map_chr(stn_lst$Stations, "StationNbr")),
      name = purrr::map_chr(stn_lst$Stations, "Name"),
      city = purrr::map_chr(stn_lst$Stations, "City"),
      county = purrr::map_chr(stn_lst$Stations, "County"),
      hmslon_chr = purrr::map_chr(stn_lst$Stations, "HmsLongitude"),
      hmslat_chr = purrr::map_chr(stn_lst$Stations, "HmsLatitude"),
      is_active_chr = purrr::map_chr(stn_lst$Stations, "IsActive")) |>
      mutate(active = (is_active_chr == "True")) |>
      mutate(lon = as.numeric(stringr::str_extract(hmslon_chr, "(?<=/ ).*")),
             lat = as.numeric(stringr::str_extract(hmslat_chr, "(?<=/ ).*"))) |>

      select(-hmslon_chr, -hmslat_chr, -is_active_chr) |>
      filter(active)

    ## Create a projected sf object
    stn_sf <- stn_tbl |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      st_transform(crs = crs)

    ## Save it to the cache folder
    if (use_cache) {
      if (!file.exists(stn_geojson) || overwrite) {
        st_write(stn_sf, dsn = stn_geojson, delete_dsn = TRUE, quiet = TRUE)
      }
    }
  }

  ## Return the sf object
  stn_sf

}






