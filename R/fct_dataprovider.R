#' DataProvider
#'  
#' @description An R6 class responsible for providing data to the application.
#' 
#' API Usage:
#' 
#' "'GET /':   Attribution and instructions of what URL paths and queries are available, along with a description of each.", 
#' "'GET /datasets/':   Metadata of each dataset, including spreadsheet *key*, *indicatorName*, *category* and *subcategory* the dataset belongs to, and the *dataprovider* and the *dataprovider_link*.", 
#' "'GET /datasets/categories/':   Shows a count of how many datasets belong to each category and subcategory.", 
#' "'GET /datasets/categorized/':   Shows which datasets belong to each category and subcategory.", 
#' "'GET /data/help/?key=key':   Shows which countries and years are available in the specified dataset.", 
#' "'GET /data/?key=key&country=country&year=year':   Gets the value for the specified country and year in the dataset.", 
#' "'GET /data/?key=key&country=country':   Gets the values for each available year in the dataset, for the specified country.", 
#' "'GET /data/?key=key&year=year':   Gets the values for each available country in the dataset, for the specified year.", 
#' "'GET /data/?key=key':   Downloads the CSV file of the whole dataset."
#'
#' @docType class
#' @importFrom tibble as_tibble
#' @importFrom magrittr `%>%`
#' @importFrom data.table rbindlist
#' @importFrom logger log_info
#' @importFrom httr GET content
#' @importFrom R6 R6Class
#'
#' @return DataProvider object
#'
#' @export

ddf_provider <- R6Class(
  "DdfProvider",
  public = list(
    repos = NULL,
    initialize = function(org = "open-numbers", log_level = INFO) {
      log_threshold(log_level, namespace = "ddf_provider")
      private$.org = org
      self$repos = private$.call_api(sprintf("orgs/%s/repos", private$.org))$name
    },
    get_all_repos = function(repos = self$repos) {
      lapply(repos, self$get_repo) %>% 
        setNames(repos) %>%
        rbindlist(idcol = "")
    },
    get_repo = function(repo) {
      endpoint <- sprintf("repos/%s/%s/contents/", private$.org, repo)
      private$.call_api(endpoint)
    },
    read_ddf = function(repo, ddf_type="", download_all=FALSE) {
      repo <- self$get_repo(repo)
      ddf_type <- paste0("ddf--", ddf_type)
      dt <- repo[grepl(ddf_type, name) & !is.na(download_url), .(name, download_url)]
      if (download_all) {
        dt[, data := lapply(download_url, fread)]
      }
      dt
    }
  ),
  private = list(
    .debug_data = structure(list(
      name = c(".gitignore", "README.md", "datapackage.json"),
      path = c(".gitignore", "README.md", "datapackage.json"),
      sha = c("3164a0816e3d1f6b6a4a58b51bb7e398b505a255", 
              "f7921d1494676ff8ce7e681765ff4ab6b08d2dad",
              "ee5fb746604d3f4485745540ae969bf1d11b40c0")),
      row.names = c(NA, -3L), class = c("data.table", "data.frame")),
    .org = "",
    .api_url = "https://api.github.com",
    .call_api = function(endpoint, base_url=private$.api_url, as_datatable=TRUE, debug=FALSE) {
      api_call <- sprintf("%s/%s", base_url, endpoint)
      log_info(sprintf("Calling API at: %s", api_call))
      if (isTRUE(debug)) {
        log_debug("Entering debugging")
        
      }
      req <- GET(api_call)
      if (req$status_code == 200) {
        log_info("Request successful")
        cont <- content(req)
        if (isTRUE(as_datatable)) {
          log_debug("Returning data.table")
          private$.response_to_datatable(cont)          
        } else {
          log_debug("Returning httr::content")
          cont
        }
      } else {
        log_error("Request unsuccessful. Error Code %s", req$status_code)
        log_error("Message:\n", paste0(content(req), collapse = "\n"))
      }
    },
    .response_to_datatable = function(response) {
      log_info("Casting response to row-bound data.table (with fill=True)")
      rbindlist(
        lapply(response, function(x) {
          as.data.table(
            x[lengths(x) == 1],
            lapply(x[lengths(x) != 1], function(x) {list(as.data.table(x))})
          )
        }),
        fill = TRUE
      )[]
    }
  ),
  active= list(
    org = function(value) {
      if (missing(value)) {
        private$.org
      } else {
        stop("org is read-only.", call. = FALSE)
      }
    },
    org_url = function(value) {
      if (missing(value)) {
        sprintf("%s/orgs/%s", private$.api_url, private$.org)
      } else {
        stop("org_url is read-only.", call. = FALSE)
      }
    },
    api_url = function(value) {
      if (missing(value)) {
        private$.api_url
      } else {
        stop("git_url is read-only.", call. = FALSE)
      }
    }
  )
)

provider <- ddf_provider$new()
repo_df<- provider$get_repo(p$repos[2])
ddf <- provider$read_ddf(p$repos[2], ddf_type = "concepts", download_all = TRUE)
ddf$data
repo_df
