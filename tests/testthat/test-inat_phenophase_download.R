test_that("inat_phenophase_download errors if not given a numeric taxon ID", {
  expect_error(inat_phenophase_download("Yucca brevifolia", years=2010:2012))
})
