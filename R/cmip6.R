#' Mounts and unmounts the CMIP6 database on a local folder
#'
#' @param user user
#' @param base_dir directory where to mount the folder
#'
#' @export
cmip_mount <- function(user, base_dir = cmip_folder_get()) {
  .check_system("sshfs")

  base_dir <- path.expand(base_dir)
  link_dir <- basename(base_dir)

  if (dir.exists(base_dir)) {
    if (!file.exists(file.path(base_dir, ".cima_cmip6"))) {
      stop(base_dir, " already exists and is not a CMIP6 folder structure.")
    } else {
      message(base_dir, " is already a CMIP6 folder. Skiping.")
      return(invisible(base_dir))
    }
  }

  server <- .cmip_server()
  folder <- .cmip_folder()

  mount_dir <- tempdir()
  dir.create(file.path(mount_dir, link_dir), mode = "775")

  # dir.create(base_dir, recursive = TRUE)
  command <- glue::glue("sshfs {user}@{server}:{folder} {mount_dir}/{link_dir}")
  out <- system(command, intern = TRUE)

  dir.create(dirname(base_dir))

  system(glue::glue("ln -s {mount_dir}/{link_dir} {base_dir}"))

  return(invisible(base_dir))
}

#' @rdname cmip_mount
#' @export
cmip_unmount <- function(base_dir = cmip_folder_get()) {
  .check_system("fusermount")
  base_dir <- path.expand(base_dir)
  system(glue::glue("fusermount -u {base_dir}"))
}


#' Lists available data
#'
#' @param base_dir directory of the CMIP6 databse
#' @export
#' @importFrom stats na.omit
cmip_available <- function(base_dir = cmip_folder_get()) {

  if (!file.exists(file.path(base_dir, ".cima_cmip6"))) {
    stop("No CMIP6 folder structure in ", base_dir, ". Mount it first wiht `cmip_mount(base_dir = ", base_dir, ").")
  }


  base_dir <- "~/DATOS/CMIP6/"
  files <- list.files(base_dir, recursive = TRUE)
  download <- vapply(files, function(f) strsplit(f, "/")[[1]][1] == "Download", TRUE)
  files <- files[!download]
  pattern <- .cmip_pattern(type = "ensemble", ext = "nc4")

  available <- unglue::unglue_data(files, pattern)
  available <- available[, c("scenario", "model", "timestep", "var", "date_start", "date_end")]
  available$file <- file.path(base_dir, files)
  available <- stats::na.omit(available)

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

  cbind(available, info)
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
  search_results
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

.cmip_pattern <- function(type = c("ensamble", "member"), ext) {
  if (missing(ext)) {
    ext <- "{ext}"
  }

  if (type[1] == "member") {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{model}_{experiment_id}_{member}_{date_start}-{date_end}.", ext)
  } else {
    pattern <- paste0("{experiment_id}/{frequency}/{variable_id}/{variable_id}_Amon_{model}_{experiment_id}_{date_start}-{date_end}.", ext)
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
  pattern <- paste0("{base_dir}/Download/Format/{type}/", .cmip_pattern("member"))

  variable_id <- result$variable_id[[1]]
  experiment_id <- .cmip_cima_experiments(result$experiment_id[[1]])
  frequency <- result$frequency[[1]]
  model <- result$source_id[[1]]
  date_start <- gsub("-", "", substr(result$datetime_start, 1, 7))
  if(length(date_start) == 0) date_start <- "9999"

  date_end <- gsub("-", "", substr(result$datetime_stop, 1, 7))
  if(length(date_end) == 0) date_end <- "9999"

  member <- result$member_id[[1]]

  type <- "wget"
  ext <- "sh"
  file <- glue::glue(pattern)

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  wget <- .cmip_save_wget_one(result, file = file)
  pass <- cmip_key_get(user = user)
  user <- paste0("https://esgf-node.llnl.gov/esgf-idp/openid/", user)
  type <- "raw"
  ext <- "nc"
  file <- glue::glue(pattern)

  if (file.exists(file)) {
    message(file, " already present. skipping.")
    return(file)
  }

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  # Change http to https (horrible hack)
  tmpdir <- tempdir()  # run script from tempdir because it creates a bunch of crap files.
  out <- suppressWarnings(system(glue::glue("cd {tmpdir} && echo {pass} | bash {wget} -d -v -H {user}"), intern = TRUE, timeout = 5))
  command <- tail(out, 2)[1]
  command <- gsub("http:", "https:", command)
  url <- tail(strsplit(command, " ")[[1]], 1)
  download.file(url, destfile = file)
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


cmip_consolidate <- function(files) {
  # download_dir <- paste0(base_dir, "/Download/Format/raw")
  # files <- list.files(download_dir, recursive = TRUE, pattern = ".nc")

  data <- unglue::unglue_data(files, paste0("{base_dir}/Download/Format/raw/", .cmip_pattern("member", ext = "nc")))
  data <- data[, setdiff(names(data),  c("variable_id.1", "experiment_id.1"))]
  data$file <- files

  uniques <- with(data, interaction(experiment_id, frequency, variable_id, model))

  out <- split(data, uniques)

  vapply(out, function(dt) {
    out_file <- glue::glue_data(dt, paste0("{base_dir}/", .cmip_pattern("ensemble", ext = "nc")))[1]
    dest_dir <- dirname(out_file)
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    in_files <- paste0(dt$file, collapse = " ")

    if (file.exists(out_file)) {
      message(out_file, " already exists. Skipping.")
      return(out_file)
    }
    system(glue::glue("ncecat --ovr -M -u ensemble {in_files} {out_file}"))
    out_file
  }, "file")
}
