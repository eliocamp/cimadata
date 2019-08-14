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

   available <- unglue::unglue_data(files, "{scenario}/{timestep}/{var}/{var}_Amon_{model}_{scenario}_{date_start}-{date_end}.nc4")
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
  query$facets  <- "mip_era,activity_id,model_cohort,product,source_id,institution_id,source_type,nominal_resolution,experiment_id,sub_experiment_id,variant_label,grid_label,table_id,frequency,realm,variable_id,cf_standard_name,data_node"
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
  wget_base <- "https://esgf-node.llnl.gov/esg-search/wget"
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


#' @importFrom utils tail
.cmip_download_one <- function(result, base_dir, user = cmip_default_user_get()) {

  pattern <- "{base_dir}/{scenario}/{timestep}/{var}/{model}/{type}/{member}_{date_start}-{date_end}.{ext}"

  var <- result$variable[[1]]
  scenario <- result$experiment_id[[1]]
  timestep <- result$frequency[[1]]
  model <- result$source_id[[1]]
  date_start <- gsub("-", "", substr(result$datetime_start, 1, 7))
  date_end <- gsub("-", "", substr(result$datetime_stop, 1, 7))
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

  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)
  }

  # Change http to https (horrible hack)
  out <- suppressWarnings(system(glue::glue("echo {pass} | bash {wget} -d -v -H {user}"), intern = TRUE, timeout = 2))
  command <- tail(out, 2)[1]
  command <- gsub("http:", "https:", command)
  log <- system(paste0(command, " -o ", file), intern = TRUE, ignore.stderr = TRUE)
}


cmip_download <- function(results, base_dir, user = cmip_default_user_get()) {
  out <- lapply(results, .cmip_download_one, base_dir = base_dir, user = user)
}
