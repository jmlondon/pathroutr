l_geom <- lapply(1:3, \(i) {
  sf::st_linestring(cbind(c(i, i + 1), c(0, 0)))
}) |>
  sf::st_sfc(crs = 4326) |> 
  data.frame()

start_pt <- sf::st_sfc(sf::st_point(c(1, 1)), crs = 4326)
end_pt <- sf::st_sfc(sf::st_point(c(4, -1)), crs = 4326)

out <- prt_extend_path(l_geom, start_pt, end_pt)


test_that("creates a sfc linestring", { 
  expect_s3_class(out, c("sfc_LINESTRING", "sfc"))
})

test_that("appends points", { 
  out_coords <- sf::st_coordinates(out)
  expect_equal(sf::st_coordinates(start_pt)[1,], out_coords[1, 1:2])
  expect_equal(sf::st_coordinates(end_pt)[1,], out_coords[nrow(out_coords), 1:2])
})

test_that("maintains input crs", { 
  expect_equal(sf::st_crs(out), sf::st_crs(l_geom$geometry))
})

test_that("reverses line if necessary", {
  start_pt_reverse <- end_pt
  end_pt_reverse <- start_pt

  out_rev <- prt_extend_path(l_geom, start_pt_reverse, end_pt_reverse)

  expect_equal(out_rev, sf::st_reverse(out))
})