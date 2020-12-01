#' Update track points with fixed geometry
#'
#' @param track_pts point geometry
#' @param segs_tbl segments table
#'
#' @return point geometry
#' @export
#'
prt_update_points <- function(track_pts, segs_tbl) {
  track_pts <- track_pts %>% sf::st_cast('POINT') %>%
    sf::st_sf() %>% tibble::rowid_to_column("id")

  pts_list <- vector(mode = "list", length = nrow(segs_tbl))

  for (i in 1:nrow(segs_tbl)) {
    fix_idx <- seq(segs_tbl$start_idx[i] + 1,segs_tbl$end_idx[i] - 1)
    fix_pts <- sf::st_sample(segs_tbl$geometry[i], size = segs_tbl$n_pts[i],
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
