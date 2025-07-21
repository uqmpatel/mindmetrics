#' @name rci_helpers
#' @aliases SEm
#' @aliases sdiff
#' @aliases RC
#' @aliases RC2
#' @aliases RCI
#'
#'
#' @title Helper functions for investigating the Reliable Change Index (RCI)
#'
#' @description
#'
#' Smaller functions that can be used quickly to calculate quantities related to the Reliable Change Index (RCI) \cr
#'
#' @param sd_norm A reference/population standard deviation
#' @param rxx The instrument test-retest reliability (typically ICC or Cronbach's alpha etc.)
#' @param SEm standard error of measurement
#' @param pre_mean A mean of pre scores
#' @param post_mean A mean of post scores
#' @param sdiff standard error of measurement of differences
#'
#'
#' @note The following functions describe various steps to calculating the RCI:
#'
#'  \itemize{
#'    \item SEm: Standard error of measurement (In Jacobson & Truax (1991) this is know as SE). For SEm - sd_norm is the Standard deviation of a normal population, or control group, or pre-treatment experimental group, rxx is the test- retest reliability, or internal consistency (E.g. ICC, Cronbach's alpha)
#'    \item Sdiff: standard error of the difference between pre and post scores. Describes the spread of the distribution (sd) of change scores, that would be expected if no actual change had occurred. An RC larger than 1 .96 would be unlikely to occur (p < .05) without actual change.
#'    \item RC: is reminiscent of an SMD but slightly different. The denominators for both an SMD and RC are spreads of data. For d it is of the change distribution, whereas for RC it is of the distribution for which no change has occurred. If RC is greater than 1.96, it is a rare observation at the 95% confidence level.
#'    \item RC2: is the same as RC, but takes the rxx, and sd_norm directly, rather than the computed sdiff
#'    \item RCI: the level required for clinical significance on the same scale as the instrument in question.
#'} \cr
#'
#'
#'
#' @source
#'
#'See [Jacobson & Truax (1991)]((https://pubmed.ncbi.nlm.nih.gov/2002127/)) outlining the Reliable Change Index (RCI). \cr
#'
#' @example rci_helpers_examples.R

# the RCI sub functions

#' @rdname rci_helpers
#' @usage SEm(sd_norm, rxx)
#' @export
SEm <- function(sd_norm, rxx){ sd_norm*sqrt(1 - rxx)}


#' @rdname rci_helpers
#' @usage sdiff(SEm)
#' @export
sdiff <- function(SEm){sqrt(2*SEm^2)}


#' @rdname rci_helpers
#' @usage RC(pre_mean, post_mean, sdiff)
#' @export
RC <- function(pre_mean, post_mean, sdiff){ (post_mean - pre_mean) / sdiff } #


#' @rdname rci_helpers
#' @usage RC2(pre_mean, post_mean, sd_norm, rxx)
#' @export
RC2 <- function(pre_mean, post_mean, sd_norm, rxx){ (post_mean - pre_mean) / sqrt(2*(sd_norm*sqrt(1 - rxx))^2) }


#' @rdname rci_helpers
#' @usage RCI(sdiff, conf.lvl)
#' @export
RCI <- function(sdiff, conf.lvl){

  if( missing(conf.lvl)  ){  conf.lvl = 0.95  }

  z.val = qnorm(1 - ((1 - conf.lvl) / 2))

  sdiff*z.val

}




