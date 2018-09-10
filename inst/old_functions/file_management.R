#----
#' Recursively source all the function stored in a specific folder
#'
#' Inspired from https://stackoverflow.com/questions/32862426/load-all-files-from-folder-and-subfolders
#' @author Thomas Goossens - pokyah.github.io
#' @param path.chr a character specifying the path to the folder where to recursively look for R files to source
#' @return sourced functions from the R files located in the specified folder
#' @export
source_files_recursively.fun <- function(path.chr) {
  dirs <- list.dirs(path.chr, recursive = FALSE)
  dirs <- dirs[ ! grepl(".git", dirs) ]
  dirs <- dirs[ ! grepl(".Rproj.user", dirs) ]

  files <- list.files(path.chr, pattern = "^.*[Rr]$", include.dirs = FALSE, full.names = TRUE)
  for (f in files)
    source(f)
  for (d in dirs)
    source_files_recursively.fun(d)
}

#----
#' List all the packages installed by user.
#'
#' Inspired from https://stackoverflow.com/questions/38481980/get-the-list-of-installed-packages-by-user-in-r#40120266
#' @author Thomas Goossens - pokyah.github.io
#' @return a dataframe containing all the installed packages and their version
#' @export
get_installed_packages.fun <- function(){
  ip = as.data.frame(installed.packages()[,c(1,3:4)])
  ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
  return(ip)
}
