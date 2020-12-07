#' Update track points with fixed geometry
#'
#' Original geometry is updated in place and (currently) no record of those points
#' that were updated is provided.
#'
#' @param track_pts original trkpts Simple Features Collection
#' @param segs_tbl output from `prt_shortpath()`
#'
#' @return trackpts with updated geometry
#' @export
#'
prt_update_points <- function(track_pts, segs_tbl) {
  track_pts <- track_pts %>% sf::st_cast('POINT') %>%
    sf::st_sf() %>% tibble::rowid_to_column("id")

  pts_list <- vector(mode = "list", length = nrow(segs_tbl))

  for (i in 1:nrow(segs_tbl)) {
    fix_idx <- seq(segs_tbl$start_idx[i] + 1,segs_tbl$end_idx[i] - 1)
    fix_pts <- sf::st_line_sample(segs_tbl$geometry[i],
                             n = segs_tbl$n_pts[i],
                             type = "regular") %>%
      sf::st_cast('POINT')
    #fix_pts <- prt_orient_line(segs_tbl$start_pt[i], fix_pts)
    pts_list[[i]] <- sf::st_sf(id = fix_idx, geometry = fix_pts )
  }

  fixed_pts <- do.call(rbind, pts_list) %>%
    tibble::as_tibble()

  track_pts_fix <- track_pts %>% tibble::as_tibble()  %>%
    dplyr::rows_update(fixed_pts, by = "id") %>%
    sf::st_sf(sf_column_name = "geometry")

  return(track_pts_fix)
}
