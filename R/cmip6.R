#' Mounts the CMIP6 database on a local folder
#'
#' @param user user
#' @param base_dir directory where to mount the folder
#'
#' @export
cmip_mount <- function(user, base_dir = cmip_folder_get()) {
  server <- .cmip_server()
  folder <- .cmip_folder()
  command <- glue::glue("sshfs {user}@{server}:{folder} {base_dir}")
  system(command)
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
