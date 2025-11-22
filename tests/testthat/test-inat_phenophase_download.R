test_that("inat_phenophase_download errors if not given a numeric taxon ID", {
  expect_error(inat_phenophase_download("Yucca brevifolia", years=2010:2012))
})

test_that("inat_phenophase_download errors if not given numeric years", {
  expect_error(inat_phenophase_download(53405, years="2010:2012"))
})

test_that("inat_phenophase_download errors if not given appropriate years", {
  expect_error(inat_phenophase_download(53405, years=2007:2009))
})

test_that("inat_phenophase_download errors returns a dataframe with expected characteristics", {
  toytrial <- inat_phenophase_download(53405, years=2012)
  expect_equal(ncol(toytrial), 8)
  expect_equal(colnames(toytrial), c("scientific_name", "latitude", "longitude", "url", "image_url", "observed_on", "phenology", "year"))
})
