read_ddf = function(repo, ddf_type="", download_all=FALSE) {
  repo <- repo_df
  ddf_type <- paste0("ddf--", ddf_type)
  dt <- repo[grepl(ddf_type, name) & !is.na(download_url), .(name, download_url)]
  if (download_all) {
    dt[, data := lapply(download_url, fread)]
  }
  dt
}
read_ddf()
repo_df