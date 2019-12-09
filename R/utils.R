#' @title WeMo Switch Port
#'
#' @description Automatically detects the port used by WeMo Switch smart plugs.
#'
#' @param ip A characted string or vector of character strings representing the
#'   IP address(es) of WeMo Switch smart plug(s).
#'
#' @param timeout Number of seconds to wait for a response until giving up. Can
#'   not be less than 1 ms (default: 0.1).
#'
#' @return A \code{\link[tibble]{tibble}} with two columns:
#'   \describe{
#'     \item{ip}{The IP address of the WeMo Switch smart plug.}
#'     \item{port}{The port at which the WeMo Switch smart plug can be accessed.
#'                 NA will be returned is no WeMo Switch is detected at this IP.}
#'   }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' wemo_PORT("192.0.1.1")
#' }
#'
#' @export
wemo_PORT <- function(ip, timeout = 0.1) {
  tib <- expand.grid(ip = factor(ip), port = c(49152, 49153, 49154, 49155))
  tib$url <- paste0("http://", tib$ip, ":", tib$port)
  tib$test <- mapply(function(url, handle) {
    tryCatch(httr::GET(url, httr::timeout(timeout))$status_code, error = function(e) 500)
  }, url = tib$url) == 404

  test <- cbind(by(tib, tib[, "ip"], function(x) {
    if (any(x$test)) {
      x$port[x$test]
    } else {
      NA
    }
  }))

  tibble::tibble(ip = row.names(test), port = test[, 1])
}
