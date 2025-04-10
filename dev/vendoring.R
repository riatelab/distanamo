#' The code of this function was copied/pasted from
#' https://github.com/PRQL/prqlc-r/blob/main/dev/vendoring.R
#' (licensed under MIT - (c) prqlr authors)
#' with minor modification to remove the vendor folder after the vendor.tar.xz has been created
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

  # config_toml_content <- stringr::str_replace_all(config_toml_content, "rust/vendor", "vendor")

  brio::write_lines(
    text = config_toml_content,
    path = config_toml_file
  )

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

vendor_crates()