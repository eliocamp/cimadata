#' Password management
#'
#' Functions to save, get and remove cmip6tinr passwords.
#'
#' @param user,password username and password. If `NULL`, they will be asked
#' interactively (recommended).
#'
#' @details#'
#' The default `NULL` value for user and password is the recommended method for
#' secuirity, as otherwise your credentials will be saved in plain text at the
#' command history.
#'
#' @name cmip_key
#' @aliases cmip_key_remove cmip_key_get cmip_key_set
NULL


#' @describeIn cmip_key Set a new user/password combination
#' @export
cmip_key_set <- function(user = NULL, password = NULL) {
  if (is.null(user) || is.null(password)) {
    user <- readline("User: ")
    if (is.null(user) | user == "") {
      stop("No user supplied.")
    }
    password <- getPass::getPass(msg = "Password: ", noblank = TRUE)
    if (is.null(password)) {
      stop("No password supplied.")
    }
  }
  if(keyring::default_backend()$name != "env") {
    keyring::keyring_unlock()
  }

  keyring::key_set_with_value(service = .cmip_service(),
                              username = user,
                              password = password)
  return(invisible(user))
}


#' @describeIn cmip_key Get password for an user.
#' @export
cmip_key_get <- function(user) {
  if(keyring::default_backend()$name != "env") {
    keyring::keyring_unlock()
  }
  keyring::key_get(service = .cmip_service(),
                   username = user)
}


cmip_key_remove <- function(user) {
  keyring::key_delete(service = .cmip_service(),
                      username = user)
}

.cmip_service <- function() {
  "cimadata_cmip"
}



#' Default users
#'
#' @param user user to use as the default (`NULL` means no default user).
#'
#' @return
#' `cmip_default_user_get()` returns the default user set with `cmip_default_user_get()`
#' or the user if there's only one user saved with `cmip_key_set()`.
#'
#' `cmip_default_user_get()` sets the default user (for the current session)
#' and returns the supplied user invisibly.
#'
#' @aliases cmip_default_user_get cmip_default_user_set
#' @name cmip_default_user
NULL

#' @describeIn cmip_default_user Sets the default user.
#' @export
cmip_default_user_set <- function(user = NULL) {
  options("CMIP.DEFAULT.USER" = user)
  return(invisible(user))
}

#' @describeIn cmip_default_user Gets the default user.
#' @export
cmip_default_user_get <- function() {
  # First priotiy: global option
  user <- getOption("CMIP.DEFAULT.USER", default = NULL)

  if (is.null(user)) {
    # Second priority: check if only one user
    all_keys <- keyring::key_list()
    cmip_keys <- all_keys[all_keys[["service"]] == .cmip_service(), ]

    if (nrow(cmip_keys) == 0) {
      warning("No users saved. Create an account at ", .cmip_url(),
              " and (optionally) use 'cmip_key_set()' to save it.")
      user <- NULL
    } else if (nrow(cmip_keys) > 1) {
      stop("Multiple users present in keyring. Use 'cmip_default_user_set()' ",
           "to set the default user manually.")
    } else {
      user <- cmip_keys[["username"]]
    }
  }

  return(user)
}

.cmip_url <- function() {
  "https://esgf-node.llnl.gov/user/add/"
}

