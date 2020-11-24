#' Re-route segments that cross barrier
#'
#' @param segs_tbl tbl from get_barrier_segments
#' @param sln SpatialLinesNetwork
#'
#' @return data frame
#' @export
#'
prt_segment_routes <- function(segs_tbl, sln) {
  start <- segs_tbl$start_node
  end <- segs_tbl$end_node

  diff_nodes <- start != end
  same_nodes <- which(!diff_nodes)

  start <- start[diff_nodes]
  end <- end[diff_nodes]

  p <- stplanr::sum_network_routes(sln = sln, start = start, end = end)

  if (length(same_nodes) > 0) {
  seg_list <- vector(mode = "list", length = length(same_nodes))
  r1 <- 1
  j <- 1
  for (i in same_nodes) {
    r2 <- i - 1
    seg_list[[j]] <- cbind(segs_tbl[r1:r2,], p[r1:r2,])
    r1 <- i + 1
    j <- j + 1
  }

  for (k in 1:length(seg_list)) {
    empty_row <- segs_tbl[same_nodes[k],]
    empty_row <- cbind(empty_row, p[1,])
    empty_row$geometry <- NA
    empty_row$ID <- NA
    empty_row$sum_length <- 0
    empty_row$pathfound <- FALSE
    seg_list[[k]] <- rbind(seg_list[[k]],empty_row)
  }
  segs_tbl <- do.call(rbind, seg_list) %>%
    sf::st_as_sf(sf_column_name = "geometry")
  warning(paste0(length(same_nodes),
                 " segments resulted in errors. check the following rows in seg_tbl: ",
                 c(same_nodes)))
  # for (i in 1:length(same_nodes)) {
  #   pt1 <- sf::st_geometry(stplanr::sln2points(sln)[same_nodes[i],])
  #   pt2 <- pt1 %>% sf::st_jitter(1)
  #   segs_tbl$geometry[same_nodes[i]] <- sf::st_multipoint(
  #     sf::st_coordinates(c(pt1,pt2))
  #     ) %>% sf::st_sfc() %>%
  #     sf::st_cast('LINESTRING')
  # }
  } else {
    segs_tbl <- cbind(segs_tbl,p) %>%
      sf::st_as_sf(sf_column_name = "geometry")
  }
  return(segs_tbl)
}
