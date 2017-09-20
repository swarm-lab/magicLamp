#' @title Check WeMo Switch State
#'
#' @description Check the ON/OFF state of WeMo Switch smart plugs.
#'
#' @param ip A characted string or vector of character strings representing the
#'   IP address(es) of WeMo Switch smart plug(s).
#'
#' @return A \code{\link[tibble]{tibble}} with two columns:
#'   \describe{
#'     \item{ip}{The IP address of the WeMo Switch smart plug}
#'     \item{state}{'ON' if the WeMo Switch is turned on, 'OFF' if it is turned
#'                  off. NA will be returned is no WeMo Switch is detected at
#'                  this IP.}
#'   }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' wemo_STATE("192.0.1.1")
#' }
#'
#' @export
wemo_STATE <- function(ip) {
  tib <- wemo_PORT(ip)
  tib$url <- paste0("http://", tib$ip, ":", tib$port, "/upnp/control/basicevent1")

  test <- cbind(by(tib, tib[, "ip"], function(x) {
    if (is.na(x$port)) {
      NA
    } else {
      response <- httr::POST(url = x$url,
                             httr::accept(''),
                             httr::content_type('text/xml; charset="utf-8"'),
                             httr::add_headers(SOAPACTION = '\"urn:Belkin:service:basicevent:1#GetBinaryState\"'),
                             body = '<?xml version="1.0" encoding="utf-8"?><s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><s:Body><u:GetBinaryState xmlns:u="urn:Belkin:service:basicevent:1"><BinaryState>1</BinaryState></u:GetBinaryState></s:Body></s:Envelope>')
      if (gsub(".*<BinaryState>*|</BinaryState>.*", "", response) == "1") {
        "ON"
      } else {
        "OFF"
      }
    }
  }))

  tibble::tibble(ip = row.names(test), state = test[, 1])
}
