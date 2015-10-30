#' @export
repositories <- function(version = NULL,
  R_version = R.Version(),
  useDevel = getOption("BiocInstaller.useDevel", FALSE)) {

  if (is.list(R_version)) {
    R_version <- paste(c(R_version$major, R_version$minor), collapse = ".")
  }

  bioc <- bioc_metadata()
  R <- gsub("(\\d+.\\d+)\\.\\d+$", "\\1", R_version)

  if (!is.null(version)) {
    version <- match.arg(version, bioc$map$bioc)
    compatible_with_R <- bioc$map$bioc[bioc$map$R == R]
    compatible_with_bioc <- bioc$map$R[bioc$map$bioc == version]
    if (!(version %in% compatible_with_R)) {
      stop(sprintf("Bioc Version: %s not compatible with R Version: %s, Compatible Version: %s",
        version, R, compatible_with_bioc),
      call. = FALSE)
    }
    return(build_repos(version))
  }

  version <- if (R >= bioc$devel || isTRUE(useDevel)) {
    bioc$devel
  } else if (R == bioc$release) {
    bioc$release
  } else {
    compatible_versions <- bioc$map[R == bioc$map$R, ]
    if (interactive()) {
      message("Which Bioc Version would you like:")
      compatible_versions$bioc[menu(compatible_versions$bioc)]
    } else {
      last_version <- compatible_versions$bioc[length(compatible_versions$bioc)]
      warning(sprintf("Multiple Bioc Versions: %s are compatible with R: %s, using Bioc Version: %s",
        paste(collapse = ", ", compatible_versions$bioc),
        R_version,
        last_version
        ), immediate. = TRUE, call. = FALSE)
      last_version
    }
  }

  build_repos(version)
}

#' @export
biocLite <- function(pkgs = c("Biobase", "IRanges", "AnnotationDbi"), ...) {
  old <- old.packages(...)
  if (NROW(old)) {
    if (interactive()) {
      message("Old packages: ", paste0("'", old[, "Package"], "'", collapse = ", "))
      ask <- switch(menu(title = "Update?", c("all", "some", "none")),
        all = FALSE,
        some = TRUE,
        none = NULL)

      if (!is.null(ask)) {
        update.packages(ask = ask, ...)
      }
    }
  }
  install.packages(pkgs, ...)
}

#' @export
set_repositories <- function(...) {
  options(repos = repositories(...))
}

mirror <- "https://bioconductor.org/packages"

#' @export
set_mirror <- function(url) {
  mirror <<- url
}

build_repos <- function(version) {
  repos <- getOption("repos", character())
  repos["CRAN"] <- repos["CRAN"] %||% "@CRAN@"
  repos["BioCsoft"] <- sprintf("%s/%s/bioc", mirror, version)
  repos["BioCann"] <- sprintf("%s/%s/data/annotation", mirror, version)
  repos["BioCexp"] <- sprintf("%s/%s/data/experiment", mirror, version)
  repos["BioCextra"] <- sprintf("%s/%s/extra", mirror, version)
  repos
}

bioc_metadata <- function(url = "https://bioconductor.org/config.yaml") {
  data <- yaml::yaml.load(paste(readLines(url), collapse = "\n"))

  list(
    devel = data$devel_version,
    release = data$release_version,
    map = data.frame(
      R = unlist(data$r_ver_for_bioc_ver),
      bioc = names(unlist(data$r_ver_for_bioc_ver)),
      row.names=NULL, stringsAsFactors = FALSE)
    )
}

`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

.onAttach <- function(libname, pkgname) {
  set_repositories()
}
