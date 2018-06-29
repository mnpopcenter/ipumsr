# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

#' Callback classes
#'
#' These classes are used to define callback behaviors that have been adapted
#' for use on IPUMS microdata extracts. Though the callbacks from the
#' readr package will work, they will not take divide out implicit decimals
#' or add value/variable labels to the data.
#'
#' \describe{
#'  \item{IpumsSideEffectCallback}{
#'    Callback function that is used only for side effects, no results are returned.
#'    Initialize with a function that takes 2 arguments x (which will be the data chunk)
#'    and an integer that indicates the position of the first observation in the chunk.
#'    If the function returns \code{FALSE}, no more chunks will be read.
#'  }
#'  \item{IpumsDataFrameCallback}{
#'  Callback function that combines each result together into a data.frame at the end.
#'    Initalize the same was as IpumsSideEffectCallback, and the results from the
#'    function will be fed into a data.frame.
#'  }
#'  \item{IpumsListCallback}{
#'    Callback function that returns a list, where each item is the result from a chunk.
#'    Initalize the same was as IpumsSideEffectCallback.
#'  }
#'  \item{IpumsBiglmCallback}{
#'    Callback function that performs a regression on the full dataset, one chunk
#'    at a time using the biglm package. Initialize with arguments \code{model} (A
#'    formula of your model and \code{prep} a function like the other callback arguments
#'    that prepares the data before running the regression. Arguments \code{type}
#'    can be either "\code{\link[biglm]{biglm}}" (the default) and
#'    "\code{\link[biglm]{bigglm}}". Other arguments are passed to biglm/bigglm.
#'  }
#'  \item{IpumsChunkCallback}{
#'    (Only needed for advanced usage) Callback interface definition, all
#'     callback functions for IPUMS data should inherit from this class, and should
#'     use private method \code{ipumsify} on the data to handle implicit decimals
#'     and value labels.
#'   }
#' }
#' @usage NULL
#' @format NULL
#' @name ipums_callback
#' @keywords internal
#' @export
IpumsChunkCallback <- R6::R6Class(
  "IpumsChunkCallback", inherit = readr::ChunkCallback,
  private = list(
    data_structure = NULL,
    all_vars = NULL,
    var_attrs = NULL,
    rec_vinfo = NULL,
    ipumsify = function(data) {
      if (is.null(private$data_structure)) return(data)
      ipumsify_data(
        data, private$data_structure, private$all_vars,
        private$var_attrs, private$rec_vinfo
      )
    }
  ),
  public = list(
    set_ipums_fields = function(data_structure, all_vars, var_attrs, rec_vinfo = NULL) {
      private$data_structure <- data_structure
      private$all_vars <- all_vars
      private$var_attrs <- var_attrs
      private$rec_vinfo <- rec_vinfo
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsSideEffectCallback <- R6::R6Class(
  "IpumsSideEffectCallback", inherit = IpumsChunkCallback,
  private = list(
    cancel = FALSE
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$cancel <- identical(result, FALSE)
    },
    continue = function() {
      !private$cancel
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsDataFrameCallback <- R6::R6Class(
  "IpumsDataFrameCallback", inherit = IpumsChunkCallback,
  private = list(
    results = list()
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$results <- c(private$results, list(result))
    },
    result = function() {
      dplyr::bind_rows(private$results)
    },
    finally = function() {
      private$results <- list()
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsListCallback <- R6::R6Class(
  "IpumsListCallback", inherit = IpumsChunkCallback,
  private = list(
    results = list()
  ),
  public = list(
    initialize = function(callback) {
      private$callback <- callback
    },
    receive = function(data, index) {
      result <- private$callback(private$ipumsify(data), index)
      private$results <- c(private$results, list(result))
    },
    result = function() {
      private$results
    },
    finally = function() {
      private$results <- list()
    }
  )
)

#' @usage NULL
#' @format NULL
#' @rdname ipums_callback
#' @export
IpumsBiglmCallback <-  R6::R6Class(
  "IpumsBiglmCallback", inherit = IpumsChunkCallback,
  private = list(
    acc = NULL,
    prep = NULL,
    model = NULL,
    biglm_f = NULL
  ),
  public = list(
    initialize = function(model, prep = function(x, pos) x, type = c("biglm", "bigglm"), ...) {
      if (!requireNamespace("biglm")) {
        stop(paste0(
          "'biglm' package required for IpumsBiglmCallback, ",
          "install using command `install.packages('biglm')`"
        ))
      }
      private$prep <- prep
      private$model <- model
      type <- match.arg(type)
      # Some hacks to improve printing of biglm sys.call
      private$biglm_f <- if (type == "biglm") {
        function(model, data) {
          biglm <- biglm::biglm
          eval(bquote(biglm(.(model), data, ...)))
        }
      } else {
        bigglm <- biglm::bigglm
        eval(bquote(biglm::biglm(.(model), data, ...)))
        bigglm(data = data, model = model, ...)
      }
    },
    receive = function(data, index) {
      data <- private$prep(private$ipumsify(data), index)
      if (is.null(private$acc)) {
        private$acc <- private$biglm_f(private$model, data)
      } else {
        private$acc <- stats::update(private$acc, data)
      }
    },
    result = function() {
      private$acc
    }
  )
)
