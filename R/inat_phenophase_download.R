# Download phenophase-annotated species records from iNaturalist
# jby 2025.11.17
#
#' Download phenophase-annotated iNaturalist records
#'
#' @param taxon Numeric taxon ID for a target taxon in the iNaturalsist database
#' @param years Vector of requested years of observation
#' @param write_out Write out a .csv file with the requested records? If TRUE, creates "data/inat_phenology_data_<taxon ID>.csv" with the same content as the returned dataframe.
#' @param max_loc_uncertainty Maximum positional uncertainty for requested records, in meters
#' @param verbose If TRUE, provide updates with each year of data downloaded.
#'
#' @returns A dataframe with simplified iNaturalist records, by phenophase
#' @export
#'
#' @examples toyon <- inat_phenophase_download(taxon=53405, years=2021)
#'
#'

inat_phenophase_download <- function(taxon=NA, years=2021:2025, write_out=FALSE, max_loc_uncertainty=1000, verbose=TRUE){

  inat_pheno_data <- data.frame(matrix(0,0,7))
  names(inat_pheno_data) <- c("scientific_name", "latitude", "longitude", "url", "image_url", "observed_on", "phenology")

  for(y in years){

    bud.y <- try(rinat::get_inat_obs(quality="research", taxon_id=taxon, annotation=c(12, 15), year=y, maxresults=1e4))
    Sys.sleep(5) # throttling under the API limit, maybe?
    flo.y <- try(rinat::get_inat_obs(quality="research", taxon_id=taxon, annotation=c(12, 13), year=y, maxresults=1e4))
    Sys.sleep(5)
    fru.y <- try(rinat::get_inat_obs(quality="research", taxon_id=taxon, annotation=c(12, 14), year=y, maxresults=1e4))
    Sys.sleep(5)
    non.y <- try(rinat::get_inat_obs(quality="research", taxon_id=taxon, annotation=c(12, 21), year=y, maxresults=1e4))
    Sys.sleep(5)


    if(class(bud.y)=="data.frame") bud.o <- bud.y |> dplyr::filter(captive_cultivated=="false", positional_accuracy < max_loc_uncertainty) |> dplyr::select(scientific_name, latitude, longitude, url, image_url, observed_on) |> dplyr::mutate(phenology="Flower Budding", year=gsub("(\\d{4})-.+","\\1", observed_on)) else bud.o <- NULL

    if(class(flo.y)=="data.frame") flo.o <- flo.y |> dplyr::filter(captive_cultivated=="false", positional_accuracy < max_loc_uncertainty) |> dplyr::select(scientific_name, latitude, longitude, url, image_url, observed_on) |> dplyr::mutate(phenology="Flowering", year=gsub("(\\d{4})-.+","\\1", observed_on)) else flo.o <- NULL

    if(class(fru.y)=="data.frame") fru.o <- fru.y |> dplyr::filter(captive_cultivated=="false", positional_accuracy < max_loc_uncertainty) |> dplyr::select(scientific_name, latitude, longitude, url, image_url, observed_on) |> dplyr::mutate(phenology="Fruiting", year=gsub("(\\d{4})-.+","\\1", observed_on)) else fru.o <- NULL

    if(class(non.y)=="data.frame") non.o <- non.y |> dplyr::filter(captive_cultivated=="false", positional_accuracy < max_loc_uncertainty) |> dplyr::select(scientific_name, latitude, longitude, url, image_url, observed_on) |> dplyr::mutate(phenology="No Evidence of Flowering", year=gsub("(\\d{4})-.+","\\1", observed_on)) else non.o <- NULL

    inat_pheno_data <- rbind(inat_pheno_data, bud.o, flo.o, fru.o, non.o)

    if(write_out){
      # insert a check here for a valid directory and filename?
      if(!file.exists("data")) dir.create("data") # make sure there's a folder to write to!
      utils::write.table(inat_pheno_data, paste("data/inat_phenology_data_", taxon, ".csv", sep=""), sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
    }

    # provide some indication of progress
    if(verbose) cat("\nDownloaded", nrow(rbind(bud.o,flo.o,fru.o,non.o)), "records from", y, "\n\n")

  } # end loop over years

  return(inat_pheno_data)

} # close function
