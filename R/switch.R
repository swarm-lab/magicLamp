#' @title Turn WeMo Switch ON
#'
#' @description Turn WeMo Switch smart plugs on.
#'
#' @param ip A characted string or vector of character strings representing the
#'   IP address(es) of WeMo Switch smart plug(s).
#'
#' @return A \code{\link[tibble]{tibble}} with two columns:
#'   \describe{
#'     \item{ip}{The IP address of the WeMo Switch smart plug}
#'     \item{success}{A logical indicating whether the WeMo Switch was
#'                    successfully turned on. NA will be returned is no WeMo
#'                    Switch is detected at this IP.}
#'   }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' wemo_ON("192.0.1.1")
#' }
#'
#' @export
wemo_ON <- function(ip) {
  tib <- wemo_PORT(ip)
  tib$url <- paste0("http://", tib$ip, ":", tib$port, "/upnp/control/basicevent1")

  test <- cbind(by(tib, tib[, "ip"], function(x) {
    if (is.na(x$port)) {
      FALSE
    } else {
      ignore <- httr::POST(url = x$url,
                           httr::accept(''),
                           httr::content_type('text/xml; charset="utf-8"'),
                           httr::add_headers(SOAPACTION = '\"urn:Belkin:service:basicevent:1#SetBinaryState\"'),
                           body = '<?xml version="1.0" encoding="utf-8"?><s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><s:Body><u:SetBinaryState xmlns:u="urn:Belkin:service:basicevent:1"><BinaryState>1</BinaryState></u:SetBinaryState></s:Body></s:Envelope>')
      TRUE
    }
  }))

  tibble::tibble(ip = row.names(test), success = test[, 1])
}


#' @title Turn WeMo Switch OFF
#'
#' @description Turn WeMo Switch smart plugs off
#'
#' @param ip A characted string or vector of character strings representing the
#'   IP address(es) of WeMo Switch smart plug(s).
#'
#' @return A \code{\link[tibble]{tibble}} with two columns:
#'   \describe{
#'     \item{ip}{The IP address of the WeMo Switch smart plug}
#'     \item{success}{A logical indicating whether the WeMo Switch was
#'                    successfully turned off. NA will be returned is no WeMo
#'                    Switch is detected at this IP.}
#'   }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' wemo_OFF("192.0.1.1")
#' }
#'
#' @export
wemo_OFF <- function(ip) {
  tib <- wemo_PORT(ip)
  tib$url <- paste0("http://", tib$ip, ":", tib$port, "/upnp/control/basicevent1")

  test <- cbind(by(tib, tib[, "ip"], function(x) {
    if (is.na(x$port)) {
      FALSE
    } else {
      ignore <- httr::POST(url = x$url,
                           httr::accept(''),
                           httr::content_type('text/xml; charset="utf-8"'),
                           httr::add_headers(SOAPACTION = '\"urn:Belkin:service:basicevent:1#SetBinaryState\"'),
                           body = '<?xml version="1.0" encoding="utf-8"?><s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"><s:Body><u:SetBinaryState xmlns:u="urn:Belkin:service:basicevent:1"><BinaryState>0</BinaryState></u:SetBinaryState></s:Body></s:Envelope>')
      TRUE
    }
  }))

  tibble::tibble(ip = row.names(test), success = test[, 1])
}
