#' @export
hiper_glm <- function(outcome, design, family = "linear") {
  supported_family <- c("linear", "logit")
  if (!(family %in% supported_family)) {
    stop(sprintf("The family %s is not supported.", family))
  }
  warning("`hiper_glm` is yet to be implemented.")
  # TODO: maximize likelihood
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}