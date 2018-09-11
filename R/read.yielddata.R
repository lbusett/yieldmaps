#' @title Read yield data
#' @description TODO
#' @param input TODO
#' @param format `character (1)` TODO
#' @param isdir `logical` TODO
#' @import data.table
#' @importFrom data.table data.table fread rbindlist setkey
#' @importFrom sp CRS
#' @importFrom RJSONIO fromJSON
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

read.yielddata <- function(input, format, isdir=NA) {

  # Import formats
  managed_formats <- fromJSON(
    system.file("extdata", "managed_formats.json", package="yieldmaps")
  )

  # Check that the source format is managed
  if (!format %in% names(managed_formats)) {
    stop(paste0("source format is not managed (accepted values: ",names(managed_formats),")."))
  }

  # Read data
  if (is.na(isdir)) {isdir <- managed_formats[[format]][["options"]][["isdir"]]}

  if (isdir) { # if input is a directory, scanning it following the filter

    # Check that directory exists
    if (!dir.exists(input)) {
      stop(paste0("input value is not recognised as a valid directory; please edit it."))
    }

    # Check file names consistence
    file_names <- list.files(input, managed_formats[[format]][["filename"]][["regex"]])
    if (length(file_names)==0) {
      stop(paste0("no files found: check that the file names agree with the standard of the format."))
    }

    # Read metadata
    file_names_elements <- data.table("filename"=file_names)
    for (i in managed_formats[[format]][["filename"]][["elements"]]) {
      file_names_elements[,c(i):=gsub(
        managed_formats[[format]][["filename"]][["regex"]],
        paste0("\\",which(managed_formats[[format]][["filename"]][["elements"]]==i)),
        filename
      )]
    }

    # Read data
    rawdata_list <- list()
    for (single_file in file_names) {
      tryCatch(
        rawdata_list[[single_file]] <- fread(file.path(input,single_file)),
        warning = print,
        error = function(e) {
          print(paste0("The file ",single_file," was not read due to the unrecognised format. Details: ",e))
        }
      )
      # Assign metadata as replicated records
      rawdata_list[[single_file]][,c(managed_formats[[format]][["filename"]][["elements"]]):=as.list(
        file_names_elements[filename==single_file,-1]
      )]
    }
    rawdata <- rbindlist(rawdata_list, use.names=TRUE, fill=TRUE)
    rm(rawdata_list); gc()

  } else { # if input is a file, read it

    # Read metadata
    file_name_elements <- NULL
    for (i in managed_formats[[format]][["filename"]][["elements"]]) {
      file_name_elements[c(i)] <- gsub(
        managed_formats[[format]][["filename"]][["regex"]],
        paste0("\\",which(managed_formats[[format]][["filename"]][["elements"]]==i)),
        basename(input)
      )
    }

    tryCatch(
      rawdata <- fread(input),
      warning = print,
      error = function(e) {
        print(paste0("The file ",input," was not read due to the unrecognised format. Details: ",e))
      }
    )
    # Assign metadata as replicated records
    rawdata[,c(managed_formats[[format]][["filename"]][["elements"]]):=as.list(file_name_elements)]
  }

  # Convert in the output format
  # FIXME questa parte va modificata sulle indicazioni di Alberto (quali campi servono; modo corretto per calcolarli)

  # requested out fields:
  # lat, lon (EPSG 4326),
  # metadata: idfield, crop, year
  # yield (commercial, t), yield_dry (dry, t), humidity (real, rel.)
  # D yield moisted, commercial humidity (parameter)
  # for filtering:
  # speed (km/h)
  # rel_width of harvest (relative to the maximum one)
  #
  if (format == "custom_bonifiche") {
    comm_humid <- 0.13 # commercial humidity to be applied to dry yield (%)
    max_width <- 9144 # maximum harvest width (mm)
    time_step <- 1 # time stes between two consecutive harvests (s)
    crs <- CRS("+init=epsg:4326")
    crop_conversion <- c(
      "Frumento Tenero" = "wheat",
      "Frumento Duro" = "wheat",
      "Mais" = "maize",
      "Orzo Autunn" = "barley",
      "Soja" = "soybean",
      "Riso Lungo" = "rice",
      "Riso Medio" = "rice",
      "Misc Pannocchie" = "maize",
      "Mais insilato" = "maize",
      "Mais pastone" = "maize"
    )
    # seas_conversion <- c("Frumento Tenero"=1, "Frumento Duro"=1, "Mais"=2, "Orzo Autunn"=1, "Soja"=2, "Riso Lungo"=2, "Riso Medio"=2,
    #                      "Misc Pannocchie"=2, "Mais insilato"=2, "Mais pastone"=2)
    outdata <- make.yield.data.table(
      rawdata[,list(
        uid=1:nrow(rawdata),
        sid=sample(nrow(rawdata)), # ID in raw order and sampled order
        lat=latitud, lon=longitud,
        idfield,
        crop=crop_conversion[crop],
        year, #seas=seas_conversion[crop], year,
        date=as.Date(data, format="%d-%m-%Y"),
        yield=resa/1E3,
        yield_dry=resa/1E3*(1-comm_humid),
        humidity=umidita/1E4, # FIXME verificare!
        speed=distanza/100/time_step*3.6,
        rel_width=larghezza/max_width,
        # filters
        f_rangev=FALSE, f_smv=FALSE, f_mins=FALSE,
        f_rangey=FALSE, f_stdy=FALSE, f_rangeq=FALSE, f_pos=FALSE,
        filter=FALSE
      )],
      comm_humid = comm_humid,
      max_width = max_width,
      time_step = time_step,
      crs = crs,
      format = format
    )

  }

  setkey(outdata@data,sid)
  return(outdata) #temp

}
