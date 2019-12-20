#' Lista datos de CMIP6 disponibles en una carpeta
#'
#' @param base_dir carpeta raiz de la base de datos
#' @export
#' @importFrom stats na.omit
cmip_available <- function(base_dir = cmip_folder_get()) {

  # if (!file.exists(file.path(base_dir, ".cima_cmip6"))) {
  # stop("No CMIP6 folder structure in ", base_dir, ". Mount it first wiht `cmip_mount(base_dir = ", base_dir, ").")
  # }


  # base_dir <- "~/DATOS/CMIP6/"
  files <- list.files(base_dir, recursive = TRUE)
  download <- vapply(files, function(f) strsplit(f, "/")[[1]][1] == "Download", TRUE)
  files <- files[!download]
  pattern <- .cmip_pattern(type = "ensemble", ext = "nc4")
  # pattern <- "{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{source_id}_{experiment_id}_{datetime_start}-{datetime_stop}.nc4"

  available <- unglue::unglue_data(files, pattern)

  available <- available[, !duplicated(gsub("\\.1", "", colnames(available)))]

  available$file <- file.path(base_dir, files)
  available <- stats::na.omit(available)

  if (requireNamespace("ncdf4", quietly = TRUE)) {
    info <- do.call(rbind, lapply(seq_len(nrow(available)), function(i) {
      nc <-  ncdf4::nc_open(available[i, ][["file"]])
      ens_len <- ncdf4::nc_open(available[i, ][["file"]])$dim$ensemble$len
      lon_res <- 360 / nc$dim$lon$len
      lat_res <- 180 / nc$dim$lat$len

      levs <- nc$dim$plev$len
      if (is.null(levs)) levs <- 1

      data.frame(n_members = ens_len,
                 lon_res = lon_res,
                 lat_res = lat_res,
                 n_levs = levs)
    }))

    available <- cbind(available, info)
  } else {
    message("ncdf4 library not installed. Install with `install.packages(\"ncdf4\") to get more information")
  }

  available$size <- file.info(available$file)$size

  return(available)
}

#' Define y obtiene la carpeta raiz de los datos CMIP6
#'
#' @param base_dir carpeta raiz de los datos de CMIP6
#'
#'@export
cmip_folder_set <- function(base_dir) {
  options(CIMADATA.CMIP6 = base_dir)
}

#' @export
#' @rdname cmip_folder_set
cmip_folder_get <- function() {
  getOption("CIMADATA.CMIP6", stop("Carpeta raiz de CMIP6 no seteada. Use cmip_folder_set()."))
}


.cmip_server <- function() {
  "pikachu.cima.fcen.uba.ar"
}

.cmip_folder <- function() {
  "/datos3/CMIP6"
}


#' Busca datos del CMIP6
#'
#' @param query una lista que define la búsqueda.
#' @param url url de búsqueda (ver Detalles)
#'
#' @details
#' La mejor forma de obtener una `query` válida es ir al portal de búsqueda
#' ([https://esgf-node.llnl.gov/search/cmip6/](https://esgf-node.llnl.gov/search/cmip6/)),
#'  realizar una búsqueda que se aproxime
#' a lo que uno quiere. Debajo del número de resultados hay un link que dice
#' "return results as JSON", copiar ese link y usar `cmip_url_to_list()` para convertir
#' eso en una lista que luego uno puede modificar. En RStudio, se puede usar
#' un AddIn para hacerlo más rápido.
#'
#' @return
#'
#' Una lista con los resultados de la búsqueda que puede ser pasada a un data.frame con
#' [as.data.frame()]
#'
#' @export
cmip_search <- function(query) {
  query$format  <- "application/solr+json"
  query$limit   <- "999"
  query$offset  <- "0"
  query$replica <- "false"

  query <- lapply(query, function(q) {
    paste0(q, collapse = ",")
  })

  search_results <- jsonlite::parse_json( httr::content(httr::GET("https://esgf-node.llnl.gov/esg-search/search",
                                                                  query = query)) )

  search_results <- search_results$response$docs
  class(search_results) <- c("cmip_results", class(search_results))
  search_results
}


#' @rdname cmip_search
#' @export
cmip_url_to_list <- function(url) {
  query <- httr::parse_url(url)$query
  no_query <- c("offset", "limit", "facets", "format")
  query <- query[!(names(query) %in% no_query)]

  return(query)
}

.cmip_parse_search <- function(results) {

  parsed <- lapply(results, function(result) {
    datetime_start <- result$datetime_start
    if(length(datetime_start) == 0) datetime_start <- NA

    datetime_stop <- result$datetime_stop
    if(length(datetime_stop) == 0) datetime_stop <- NA

    member <- unglue::unglue(result$member_id[[1]], "r{member}i{init}p{physics}f{forcing}")[[1]]

    data.frame(
      source_id = result$source_id[[1]],
      experiment_id =  result$experiment_id[[1]],
      forcing_index = member$forcing,
      physics_index = member$physics,
      initialization_index = member$init,
      realization_index = member$member,
      frequency =  result$frequency[[1]],
      datetime_start = datetime_start,
      datetime_stop = datetime_stop,
      variable_id = result$variable_id[[1]],
      nominal_resolution = result$nominal_resolution[[1]],
      grid_label = result$grid_label[[1]],
      size = result$size/1024/1024
    )
  })

  parsed <- do.call(rbind, parsed)
  parsed
}


#' @export
as.data.frame.cmip_results <- function(x, ...) {
  .cmip_parse_search(x)
}

.cmip_save_wget_one <- function(result, file, force_https = FALSE) {
  wget_base <- glue::glue("https://{result$index_node}/esg-search/wget")

  script <- httr::content(httr::GET(wget_base,
                                    query = list(distrib = "false",
                                                 dataset_id = result$id)))

  # Force https
  if (force_https) {
    script <- strsplit(script, "\n")[[1]]
    file_lines <- grep("EOF--dataset.file.url.chksum_type.chksum", script)
    file_lines <- seq(file_lines[1], file_lines[2])
    file_lines <- file_lines[-c(1, length(file_lines))]
    script[file_lines] <- gsub("http:", "https:", script[file_lines])
  }

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  writeLines(script,
             con = file)
  return(file)
}


.cmip_pattern <- function(type = c("ensamble", "member"), ext = "{ext}") {
  if (type[1] == "member") {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{source_id}_{experiment_id}_r{realization_index}i{initialization_index}p{physics_index}f{forcing_index}_{grid_label}_{datetime_start}-{datetime_stop}.", ext)
  } else {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{source_id}_{experiment_id}_i{initialization_index}p{physics_index}f{forcing_index}_{grid_label}_{datetime_start}-{datetime_stop}.", ext)
  }

  return(pattern)
}


.cmip_cima_experiments <- function(experiment_id) {
  map <- c("historical"   = "Historical",
           "hist-GHG"     = "HistoricalGHG",
           "hist-nat"     = "HistoricalNat",
           "hist-stratO3" = "HistoricalO3",
           "hist-sol"     = "HistoricalSol")

  out <- map[experiment_id]
  if (is.na(out)) {
    out <- experiment_id
  }

  return(unname(out))
}



#' Descarga datos de CMIP6
#'
#' @param results una lista de resultados devuelta por [cmip_search()]
#' @param base_dir carpeta raiz de la base de datos de CIP6
#' @param user usuario para autenticar (ver [cmip_key_set])
#' @param system_config comandos de sistema para correr antes de iniciar las descargas
#' (por ejemplo, para setear el proxy).
#' @param progress_bar Mostrar una barra de progreso?
#'
#' @details
#' Los archivos se descargan en una estructura de carpetas estándard:
#' \[base_dir\]/Download/Format/Data_used/\{experiment_id\}/\{frequency\}/\{variable_id\}/\{variable_id\}_Amon_\{source_id\}_\{experiment_id\}_r\{realization_index\}i\{initialization_index\}p\{physics_index\}f\{forcing_index\}_\{grid_label\}_\{datetime_start\}-\{datetime_stop\}.\{ext\}
#'
#'
#' @return
#' Un vector de caracteres con los archivos descargados (que puede pasarse directamente a [cmip_consolidate()])
#'
#' @export
cmip_download <- function(results, base_dir = cmip_folder_get(), user = cmip_default_user_get(),
                          system_config = "", progress_bar = TRUE) {
  # browser()
  downloaded_files <- rep(NA_character_, length = length(results))
  pass <- cmip_key_get(user = user)
  user <- paste0("https://esgf-node.llnl.gov/esgf-idp/openid/", user)

  if (system_config != "") {
    system_config <- paste0(system_config, " && ")
  }

  on.exit({
    # If already started downloading
    if (exists("dow")) {
      if (dow$is_alive()) {
        dow$kill()
        log <- "User interrupt"
        file.remove(file)
      }  else if (dow$get_result()[length(dow$get_result())] != "done") {
        warning("Errors encountered when downloading\n", file, "\nLog dumbed into ", log_file)
        log <- dow$get_result()
        file.remove(file)  # remove unfinished file
      }

      dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
      writeLines(log, con = log_file)
    }

    return(downloaded_files)
  })


  for (i in seq_along(results)) {
    # browser()
    result <- results[[i]]

    data <- as.data.frame.cmip_results(list(result))

    data$base_dir <- base_dir

    data$datetime_start <- gsub("-", "", substr(data$datetime_start, 1, 7))
    data$datetime_stop <- gsub("-", "", substr(data$datetime_stop, 1, 7))

    pattern <- paste0("{base_dir}/Download/Format/{type}/", .cmip_pattern("member"))

    data$type <- "Data_used"
    data$ext <- "nc"
    file <- glue::glue_data(data, pattern)

    # if (file.exists(file)) {
    #   message(file, " already present. skipping.")
    #   return(file)
    # }

    data$type <- "Scripts_used"
    data$ext <- "sh"
    wget <- glue::glue_data(data, pattern)

    if (!dir.exists(dirname(file))) {
      dir.create(dirname(file), recursive = TRUE)
    }

    if (!file.exists(wget)) {
      wget <- .cmip_save_wget_one(result, file = wget)
    }


    type <- "Data_used"
    ext <- "nc"

    data$ext = "txt"
    data$type = "log"
    log_file <- glue::glue_data(data, pattern)

    if (!dir.exists(dirname(file))) {
      dir.create(dirname(file), recursive = TRUE)
    }


    command <- glue::glue("{system_config} cd {dirname(file)} && echo {pass} | bash {wget} -d -v -i -H {user}")

    pass <- "PASSWD"

    command_mock <- glue::glue("{system_config} cd {dirname(file)} && echo {pass} | bash {wget} -d -v -i -H {user}")
    message_time("Downloading ", file, " with command:\n", command_mock)
    dow <- callr::r_bg(function(command) system(command, intern = TRUE), args = list(command = command))

    if (progress_bar == TRUE) {
      pbar <- progress::progress_bar$new(format = "[:bar] :percent :rate eta: :eta", total = result$size, show_after = 1)
      while (dow$is_alive()) {
        if (file.exists(file) & !pbar$finished) {
          pbar$update(ratio = file.info(file)$size/result$size)
        }
      }
      pbar$terminate()
    } else {
      dow$wait()
    }

    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    writeLines(dow$get_result(), con = log_file)
    remove(dow)

    downloaded_files[i] <- file
  }
}

#' Calcula el tamaño total de una búsqueda en megabites
#'
#' @inheritParams cmip_download
#'
#' @export
cmip_size <- function(results) {
  res <- sum(vapply(results, function(r) r$size, FUN.VALUE = 1))/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}

#' @export
print.cmip_size <- function(x, ...) {
  cat(signif(x, 3), "Mb")
}

#' Concatena los archivos NetCDF de distintos miembros del mismo experimento
#'
#' @param files vector con los archivos a consolidar (por ejemplo, la dalida de [cmip_download()]).
#' Si es `NULL`, consolida todos los archivos en `base_dir`.
#' @param base_dir carpeta raiz de la base de datos. Sólo usado si `files` es `NULL`.
#'
#'
#' @return
#' Un vector de caracteres con los archivos consolidados.
#'
#' @export
cmip_consolidate <- function(files = NULL, base_dir) {
  if (is.null(files)) {
    download_dir <- paste0(base_dir, "/Download/Format/Data_used")
    files <- list.files(download_dir, recursive = TRUE, pattern = ".nc", full.names = TRUE)
  }
  files <- files[!is.na(files)]

  data <- unglue::unglue_data(files, paste0("{base_dir}/Download/Format/Data_used/", .cmip_pattern("member", ext = "nc")))
  data <- data[, setdiff(names(data),  c("variable_id.1", "experiment_id.1"))]
  data$file <- files
  data$remove <- FALSE

  uniques <- with(data, interaction(experiment_id, frequency, variable_id, source_id,
                                    initialization_index, physics_index, forcing_index,
                                    grid_label, drop = TRUE))
  out <- split(data, uniques)
  unlist(lapply(out, function(dt) {

    on.exit({
      #Remove temporaty files, if present
      tempfiles <- list.files(unique(dt$base_dir), pattern = "*.ncecat.tmp",
                              recursive = TRUE, full.names = TRUE)
      file.remove(tempfiles)

      # Remove other files
      file.remove(dt[dt$remove == TRUE, ]$file)
    })

    if (nrow(dt) == 0) {
      return(NULL)
    }
    # browser()

    dt_future <- dt
    dt_future$datetime_start <- min(dt_future$datetime_start)
    dt_future$datetime_stop <- max(dt_future$datetime_stop)
    out_file <- glue::glue_data(dt_future, paste0("{base_dir}/", .cmip_pattern("ensemble", ext = "nc4")))[1]

    if (file.exists(out_file)) {
      message_time(out_file, " already exists. Skipping.")
      return(out_file)
    }

    message_time(paste0(c("Processing files:", dt$file), collapse = "\n"))

    # Join dates
    members <- split(dt, dt$realization_index)
    members <- lapply(members, function(m) {
      if (nrow(m) == 1) {
        return(m)
      }
      in_files <- paste0(m$file, collapse = " ")

      m$datetime_start <- min(m$datetime_start)
      m$datetime_stop <- max(m$datetime_stop)
      m <- m[1, ]
      out_file <- glue::glue_data(m, paste0("{base_dir}/", .cmip_pattern("member", ext = "nc")))[1]

      m$file <- out_file
      m$remove <- TRUE

      if (file.exists(out_file)) {
        message_time("Member #", unique(m$realization_index), " already concatenated. Skipping.")
        return(m)
      }

      dest_dir <- dirname(out_file)
      if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
      }


      command <- glue::glue("ncrcat --ovr {in_files} {out_file}")
      message_time("Concatenating member #", unique(m$realization_index), " with command:\n", command)
      system(command)
      return(m)
    })

    dt <- do.call(rbind, members)

    dest_dir <- dirname(out_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    in_files <- paste0(dt$file, collapse = " ")
    command <- glue::glue("ncecat --ovr -M -4 -u ensemble {in_files} {out_file}")
    message_time("Merging members into ", out_file, " with command: \n", command)
    system(command)


    out_file
  }))
}



message_time <- function(..., domain = NULL, appendLF = TRUE) {
  text <- paste0(...)
  time <- paste0("[", Sys.time(), "]")
  message(paste0(time, ": ", text), domain = domain, appendLF = appendLF)
}


