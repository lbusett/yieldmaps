#' @title Read yield data
#' @description TODO
#' @param input TODO
#' @param format `character (1)` TODO
#' @import data.table
#' @importFrom data.table data.table fread rbindlist setkey
#' @importFrom sp CRS
#' @importFrom RJSONIO fromJSON
#' @importFrom stringr str_pad
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

read.yielddata <- function(input, format) {

  # Import formats
  managed_formats <- fromJSON(
    system.file("extdata", "managed_formats.json", package="yieldmaps")
  )

  # Check that the source format is managed
  if (!format %in% names(managed_formats)) {
    stop(paste0("source format is not managed (accepted values: ",names(managed_formats),")."))
  }

  # Read data

  rawdata <- foreach(j = seq_len(nrow(input)), .combine=rbind) %do% {

    sel_input <- input[j,]

    # Read metadata
    file_name_elements <- NULL
    for (i in managed_formats[[format]][["filename"]][["elements"]]) {
      file_name_elements[c(i)] <- gsub(
        managed_formats[[format]][["filename"]][["regex"]],
        paste0("\\",which(managed_formats[[format]][["filename"]][["elements"]]==i)),
        basename(sel_input$name)
      )
    }

    tryCatch(
      sel_rawdata <- fread(sel_input$datapath),
      warning = print,
      error = function(e) {
        print(paste0("The file ",input," was not read due to the unrecognised format. Details: ",e))
      }
    )

    # Assign metadata as replicated records
    sel_rawdata[,c(managed_formats[[format]][["filename"]][["elements"]]):=as.list(file_name_elements)]


  } # end of sel_input FOR cycle




  # # TODO import shapefiles
  #
  # # cycle on files
  # for (sel_input in input) {
  #
  #   # Read metadata
  #   file_name_elements <- NULL
  #   for (i in managed_formats[[format]][["filename"]][["elements"]]) {
  #     file_name_elements[c(i)] <- gsub(
  #       managed_formats[[format]][["filename"]][["regex"]],
  #       paste0("\\",which(managed_formats[[format]][["filename"]][["elements"]]==i)),
  #       basename(sel_input)
  #     )
  #   }
  #
  #   tryCatch(
  #     sel_rawdata <- sf::st_read(sel_input),
  #     warning = print,
  #     error = function(e) {
  #       print(paste0("The file ",input," was not read due to the unrecognised format. Details: ",e))
  #     }
  #   )
  #
  #
  #   names(sel_rawdata)
  #
  #   out_selrawdata <- sel_rawdata[,"geometry"]
  #   for (sel_regex in managed_formats[[format]][["content"]][["fields"]]) {
  #     out_selrawdata[,] <- grep(sel_regex, names(sel_rawdata))[1]
  #   }
  #   sel_rawdata[,sel_rawdata_ncols[!is.na(sel_rawdata_ncols)]]
  #
  #
  #
  #   # Assign metadata as replicated records
  #   rawdata[,c(managed_formats[[format]][["filename"]][["elements"]]):=as.list(file_name_elements)]
  #
  # } # end of input FOR cycle
  #


  ## Add leading zeros to idfield
  rawdata[,idfield:=paste0(
    str_pad(gsub("(^[0-9]+)([a-zA-Z]?$)","\\1",idfield), 4, "left", "0"),
    gsub("(^[0-9]+)([a-zA-Z]?$)","\\2",idfield)
  )]

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

  setkey(outdata@datatable,sid)
  return(outdata) #temp

}
