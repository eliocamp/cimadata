#' Mounts and unmounts the CMIP6 database on a local folder
#'
#' @param user user
#' @param base_dir directory where to mount the folder
#' @param tunnel whether to connect using a tunnel.
#'
#' @export
cmip_mount <- function(user, base_dir = cmip_folder_get(), tunnel = FALSE) {
  .check_system("sshfs")
  .check_system("ssh")
  base_dir <- path.expand(base_dir)
  # link_dir <- basename(base_dir)

  # if (dir.exists(base_dir)) {
  #   if (!file.exists(file.path(base_dir, ".cima_cmip6"))) {
  #     stop(base_dir, " already exists and is not a CMIP6 folder structure.")
  #   } else {
  #     message(base_dir, " is already a CMIP6 folder. Skiping.")
  #     return(invisible(base_dir))
  #   }
  # }

  server <- .cmip_server()
  folder <- .cmip_folder()

  mount_dir <- tempdir()
  mount_dir <- base_dir
  dir.create(file.path(mount_dir), mode = "775")

  if (tunnel) {
    command <- glue::glue("ssh {user}@portal.cima.fcen.uba.ar -L 4567:{server}:22 -N")
    # system(command)
    tunnel_proc <- callr::r_bg(function(command) system(command), args = list(command = command))
    Sys.sleep(3L)
    command <- glue::glue("sshfs -p 4567 {user}@localhost:/datos3/CMIP6 {base_dir}")
    bg <- callr::r_bg(function(command) system(command), args = list(command = command))
    # system(command)
    mount_point <- list(dir         = base_dir,
                        unmount     = glue::glue("fusermount -u {base_dir}"),
                        tunnel_proc = tunnel_proc)
  } else {
    # dir.create(base_dir, recursive = TRUE)
    command <- glue::glue("sshfs {user}@{server}:{folder} {mount_dir}")
    system(command, intern = TRUE)

    mount_point <- list(dir = base_dir,
                        unmount = glue::glue("fusermount -u {base_dir}"))
  }
  # system(glue::glue("ln -s {mount_dir}/{link_dir} {base_dir}"))

  return(mount_point)
}

#' @rdname cmip_mount
#' @export
cmip_unmount <- function(mount_point) {
  .check_system("fusermount")

  system(mount_point$unmount)

  if (!is.null(mount_point$tunnel_proc)) {
    mount_point$tunnel_proc$kill()
  }
}


#' Lists available data
#'
#' @param base_dir directory of the CMIP6 databse
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
  }

  return(available)
}

#' Sets and gets CMIP6 folder
#'
#' @param base_dir directory of the CMIP6 databse
#'
#'@export
cmip_folder_set <- function(base_dir) {
  options(CIMADATA.CMIP6 = base_dir)
}

#' @export
#' @rdname cmip_folder_set
cmip_folder_get <- function() {
  getOption("CIMADATA.CMIP6", "~/CMIP6")
}


.cmip_server <- function() {
  "pikachu.cima.fcen.uba.ar"
}

.cmip_folder <- function() {
  "/datos3/CMIP6"
}


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
      size = result$size/1024/1024
    )
  })

  parsed <- do.call(rbind, parsed)
  parsed
}



as.data.frame.cmip_results <- function(object) {
  .cmip_parse_search(object)
}

.cmip_save_wget_one <- function(result, base_dir, file) {
  wget_base <- glue::glue("https://{result$index_node}/esg-search/wget")

  script <- httr::content(httr::GET(wget_base,
                                    query = list(distrib = "false",
                                                 dataset_id = result$id)))
  script_name <- paste0(result[["instance_id"]], ".sh")

  if (missing(file)) {
    file <- file.path(base_dir, script_name)
  }
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  writeLines(script,
             con = file)
  return(file)
}


cmip_save_wget <- function(results, base_dir, wait = 100) {
  out <- lapply(results, function(r) {
    file <- .cmip_save_wget_one(r, base_dir = base_dir)
    Sys.sleep(wait/1000)
    return(file)
  })
}

.cmip_pattern <- function(type = c("ensamble", "member"), ext = "{ext}") {
  if (type[1] == "member") {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{source_id}_{experiment_id}_r{realization_index}i{initialization_index}p{physics_index}f{forcing_index}_{datetime_start}-{datetime_stop}.", ext)
  } else {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{source_id}_{experiment_id}_i{initialization_index}p{physics_index}f{forcing_index}_{datetime_start}-{datetime_stop}.", ext)
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

#' @importFrom utils tail
.cmip_download_one <- function(result, base_dir, user = cmip_default_user_get()) {
  data <- as.data.frame.cmip_results(list(result))

  data$base_dir <- base_dir

  data$datetime_start <- gsub("-", "", substr(data$datetime_start, 1, 7))
  data$datetime_stop <- gsub("-", "", substr(data$datetime_stop, 1, 7))

  pattern <- paste0("{base_dir}/Download/Format/{type}/", .cmip_pattern("member"))

  data$type <- "raw"
  data$ext <- "nc"
  file <- glue::glue_data(data, pattern)

  if (file.exists(file)) {
    message(file, " already present. skipping.")
    return(file)
  }

  data$type <- "wget"
  data$ext <- "sh"
  wget_file <- glue::glue_data(data, pattern)

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  wget <- .cmip_save_wget_one(result, file = wget_file)
  pass <- cmip_key_get(user = user)
  user <- paste0("https://esgf-node.llnl.gov/esgf-idp/openid/", user)
  type <- "raw"
  ext <- "nc"


  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  # Change http to https (horrible hack)
  tmpdir <- tempdir()  # run script from tempdir because it creates a bunch of crap files.
  out <- suppressWarnings(system(glue::glue("cd {tmpdir} && echo {pass} | bash {wget} -d -v -i -H {user}"),
                                 intern = TRUE, timeout = 3))
  # browser()
  command <- tail(out, 2)[1]
  command <- glue::glue("{command} -o {tempfile()} -O {file}")
  # browser()
  message("Downloading ", file)
  dow <- callr::r_bg(function(command) system(command), args = list(command = command))
  on.exit({
    if (dow$is_alive()) {
      dow$kill()         # kill process
      file.remove(file)  # remove unfinished file
    } else if (dow$get_result() != 0) {
      file.remove(file)  # remove unfinished file
    }
    file
  })
  # dow$wait()

  pbar <- progress::progress_bar$new(format = "[:bar] :percent :rate eta: :eta", total = result$size, show_after = 1)
# browser()
  while (dow$is_alive()) {
    Sys.sleep(0.1)
    if (file.exists(file)) {
      pbar$update(ratio = file.info(file)$size/result$size)
    }
  }
  pbar$update(ratio = 1)
  pbar$terminate()

  # system(command, intern = TRUE)

  # download.file(url, destfile = file)
  return(file)
  # log <- invisible(system(paste0(command, " -O ", file), intern = TRUE))
}


cmip_download <- function(results, base_dir, user = cmip_default_user_get()) {
  files <- vapply(results, .cmip_download_one, "a", base_dir = base_dir, user = user)
  return(files)
}


cmip_size <- function(results) {
  res <- sum(vapply(results, function(r) r$size, FUN.VALUE = 1))/1024/1024
  class(res) <- c("cmip_size", class(res))
  res
}


print.cmip_size <- function(object) {
  cat(signif(object, 3), "Mb")
}


cmip_consolidate <- function(files = NULL, base_dir) {
  if (is.null(files)) {
    download_dir <- paste0(base_dir, "/Download/Format/raw")
    files <- list.files(download_dir, recursive = TRUE, pattern = ".nc", full.names = TRUE)
  }

  data <- unglue::unglue_data(files, paste0("{base_dir}/Download/Format/raw/", .cmip_pattern("member", ext = "nc")))
  data <- data[, setdiff(names(data),  c("variable_id.1", "experiment_id.1"))]
  data$file <- files

  uniques <- with(data, interaction(experiment_id, frequency, variable_id, source_id, initialization_index, physics_index, forcing_index))

  out <- split(data, uniques)

  unlist(lapply(out, function(dt) {
    if (nrow(dt) == 0) {
      return(NULL)
    }
    out_file <- glue::glue_data(dt, paste0("{base_dir}/", .cmip_pattern("ensemble", ext = "nc")))[1]
    out_file4 <- glue::glue_data(dt, paste0("{base_dir}/", .cmip_pattern("ensemble", ext = "nc4")))[1]

    dest_dir <- dirname(out_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    in_files <- paste0(dt$file, collapse = " ")

    if (file.exists(out_file4)) {
      message(out_file4, " already exists. Skipping.")
      return(out_file4)
    }

    if (!dir.exists(dirname(out_file4))) {
      dir.create(dirname(out_file4), recursive = TRUE)
    }

    system(glue::glue("ncecat --ovr -M -u ensemble {in_files} {out_file}"))

    system(glue::glue("nccopy -k nc4 -d4 -s {out_file} {out_file4}"))
    file.remove(out_file)
    out_file
  }))
}
