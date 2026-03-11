# Load all package source files so tests can run without installing the package.
# testthat calls this automatically before running any test files.

pkg_root <- system.file(package = "nrsaqaapp")
if (nzchar(pkg_root)) {
  # Installed — nothing needed, namespace is already loaded
} else {
  # Development mode — source all R files
  r_files <- list.files(
    file.path(rprojroot::find_package_root_file(), "R"),
    pattern = "\\.R$", full.names = TRUE
  )
  invisible(lapply(r_files, source))
}
