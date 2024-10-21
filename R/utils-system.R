os_unix <- function() {
  .Platform$OS.type == "unix"
}

os_windows <- function() {
  .Platform$OS.type == "windows"
}

os_linux <- function() {
  grepl("linux", tolower(Sys.info()["sysname"]))
}
