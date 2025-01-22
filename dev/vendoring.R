#' The code of this function was copied/pasted from
#' https://github.com/PRQL/prqlc-r/blob/main/dev/vendoring.R
#' (licensed under MIT - (c) prqlr authors)
#' with minor modification to :
#' - remove the vendor folder after the vendor.tar.xz has been created
#' - replace cr/crlf line end by lf in savvy-bindgen/src/gen/templates/Makevars.in
#'   (and any other file if necessary)
vendor_crates <- function(path = ".") {
  src_dir <- rprojroot::find_package_root_file("src", path = path)

  out_file <- file.path(src_dir, "rust", "vendor.tar.xz")
  config_toml_file <- file.path(src_dir, "rust", "vendor-config.toml")

  vendor_rel_path <- file.path("rust", "vendor")

  withr::local_dir(src_dir)

  config_toml_content <- processx::run(
    "cargo",
    c(
      "vendor",
      "--locked",
      "--manifest-path", file.path("rust", "Cargo.toml"),
      vendor_rel_path
    )
  )$stdout

  config_toml_content <- stringr::str_replace_all(config_toml_content, "rust/vendor", "vendor")

  brio::write_lines(
    text = config_toml_content,
    path = config_toml_file
  )

  # We need to replace CR/CRLF line endings by LF in
  # src/rust/vendor/savvy-bindgen/src/gen/templates/Makevars.in in an attempt to fix
  # https://github.com/riatelab/distanamo/actions/runs/12913965346/job/36012421388#step:6:132
  find_and_replace_crlf(file.path(src_dir, "rust", "vendor"))

  withr::local_dir(file.path(src_dir, vendor_rel_path))
  withr::local_envvar(c(XZ_OPT = "-9"))
  processx::run(
    "tar",
    c(
      "-c",
      "-f", out_file,
      "--xz",
      "--sort=name",
      "--mtime=1970-01-01",
      "--owner=0",
      "--group=0",
      "--numeric-owner",
      "."
    )
  )

  unlink(file.path(src_dir, "rust", "vendor"), recursive = TRUE)
}

replace_crcrlf <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  content <- stringr::str_replace_all(content, "\r\n|\r", "\n")
  writeLines(content, file_path, sep = "\n")
}

find_and_replace_crlf <- function(directory_path) {
  files_and_dirs <- list.files(directory_path, full.names = TRUE)

  for (item in files_and_dirs) {
    if (file.info(item)$isdir) {
      find_and_replace_crlf(item)
    } else {
      if (startsWith(basename(item), "Makevars")) {
        replace_crcrlf(item)
      }
    }
  }
}

vendor_crates()