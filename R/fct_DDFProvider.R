#' DDFProvider
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
#' @importFrom magrittr `%>%`
#' @importFrom httr GET content
#' @importFrom R6 R6Class
#' @import logger
#' @import data.table
#'
#' @return DDFProvider object
#'
#' @export

DDFProvider <- R6Class(
  "DDFProvider",
  public = list(
    
    #' @description 
    #' initalize
    #' @param org GitHub Organization
    #' @param log_level Log Level, see logger::log_level()
    #' @param debug Enable debugging mode 
    #' @return DDFProvider object
    #' 
    initialize = function(org = "open-numbers", log_level = INFO, debug=FALSE) {
      log_threshold(log_level)
      private$.org = org
      private$.reset(debug = debug)
      self
    },
    #' @description 
    #' get_all_repos
    #' @param repos List of repositories to get
    #' @return data.table
    #' 
    get_all_repos = function(repos = NULL) {
      if (is.null(repos)) {
        repo_list <- private$.repos$name
      } else {
        stopifnot(is.character(repos))
        repo_list <- repos
      }
      
      private$.data <- lapply(repo_list, self$get_repo) %>% 
        setNames(repo_list) %>%
        rbindlist(idcol = "repository")
    },
    #' get_repo
    #' @param repo Name of repository to get
    #' @return data.table
    #' 
    get_repo = function(repo) {
      if (is.null(private$.data[[repo]]) || isTRUE(force)) {
        endpoint <- sprintf("repos/%s/%s/contents/", private$.org, repo)
        private$.data[[repo]] <- private$.call_api(endpoint)
      } 
      private$.data[[repo]]
    },
    #' read_ddf
    #' @param repo Name of repository to read DDF files from
    #' @param resource DDF-resource to download, default to "datapoints"
    #' @param prefetch Prefetch all DDF-resources as a column of data.tables
    #' @return data.table
    #' 
    get_ddf = function(repo, resource=c("datapoints", "concepts", "entities"), prefetch=FALSE) {
      resource <- match.arg(resource)
      repo <- self$get_repo(repo)
      resource <- paste0("ddf--", resource)
      dt <- repo[grepl(resource, name) & !is.na(download_url), .(name, download_url)]
      dt[, data := lapply(download_url, fread)]
      setcolorder(dt, c("name", "data"))
      dt
    },
    #' log
    #' @param msg Message to log
    #' @param log_level Log Level
    #' 
    log = function(msg = "Hello!", log_level = INFO) {
      print(msg)
    }
  ),
  private = list(
    .repos = NULL,
    .data = NULL,
    .ddf = NULL,
    .debug = FALSE,
    .org = "",
    .api_url = "https://api.github.com",
    
    .reset = function(debug=FALSE) {
      if (isTRUE(debug)) {
        private$.debug = TRUE
        log_threshold(DEBUG)
        repos <- debugging
      } else {
        repos <- private$.call_api(sprintf("orgs/%s/repos", private$.org))
      }
      private$.repos <- repos[, .(name, description, size, created_at, 
                                  updated_at, watchers, language, clone_url)]
      
      private$.data = vector("list", length = length(private$.repos$name)) %>% 
        setNames(private$.repos$name)
    },
    
    .call_api = function(endpoint, 
                         base_url=private$.api_url, 
                         as_datatable=TRUE, 
                         debug=FALSE) {
      api_call <- sprintf("%s/%s", base_url, endpoint)
      log_info(sprintf("Calling API at: %s", api_call))
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
  active = list(
    #' @field repos List of repositories in GitHub Organization
    repos = function(value) {
      if (missing(value)) {
        private$.repos
      } else {
        stop("repos is read-only.", call. = FALSE)
      }
    },
    #' @field org GitHub Organization
    org = function(value) {
      if (missing(value)) {
        private$.org
      } else {
        stop("org is read-only.", call. = FALSE)
      }
    },
    #' @field org_url Organization URL
    org_url = function(value) {
      if (missing(value)) {
        sprintf("%s/orgs/%s", private$.api_url, private$.org)
      } else {
        stop("org_url is read-only.", call. = FALSE)
      }
    },
    #' @field api_url API URL
    api_url = function(value) {
      if (missing(value)) {
        private$.api_url
      } else {
        stop("api_url is read-only.", call. = FALSE)
      }
    },
    #' @field data Data
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        stop("data is read-only.", call. = FALSE)
      }
    }
  )
)
