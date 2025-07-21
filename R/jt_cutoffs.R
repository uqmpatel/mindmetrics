#' @name jt_cutoffs
#' @aliases jt_a
#' @aliases jt_b
#' @aliases jt_c
#'
#' @title Cutoffs functions: jt.a, jt.b, jt.c
#'
#' @description
#' Cutoffs described by [Jacobson and Truax (1991)](https://pubmed.ncbi.nlm.nih.gov/2002127/)
#'
#' @param vec Instrument scores for sample of interest.
#' @param m0 Mean instrument score among a well functioning or normal population.
#' @param s0 Standard deviation of instrument score among a well functioning or normal population.
#' @param m1 Mean of pretest experimental or pretest control groups. If `vec` is supplied, `m1` will be calculated automatically.
#' @param s1 Mean of pretest experimental or pretest control groups. If `vec` is supplied, `s1` will be calculated automatically.
#' @param equal.variance If `TRUE`, only `m0` & `m1` (or `vec`) are required. If `FALSE`, function requires: `m0`, `s0`, `m1` & `s1`. Defaults to `TRUE`.
#'
#'
#' @note The functions `jt.a()`, `jt.b()`, and `jt.c()` each return a threshold which specify recovery in a population. \cr
#'
#' \itemize{
#'  \item `jt.a()` has the advantage of not requiring referent population norms. When distributions of well and unwell populations are overlapping, `jt_a()` can potentially be quite conservative. However, when distributions are non-overlapping, `jt_a()` will not be conservative enough.
#'  \item `jt.b()` & `jt.c()` both require artefacts from normal populations
#'  \item  if distributions are overlapping then threshold `jt.b()` is rather lenient. If non-overlapping, `jt.b()` can potentially be too stringent, but provides confidence of recovery.
#'  } \cr
#'
#' See [Jacobson & Truax's 1991]((https://pubmed.ncbi.nlm.nih.gov/2002127/)) paper outlining the Reliable Change Index (RCI).
#'
#'
#' #' @example rci_helpers_examples.R
#'
#'
#'
# jt_a ----
#' @rdname jt_cutoffs
#' @export
jt_a <-  function(vec, m1, s1, higherIsBetter){

  if ( missing(m1) ){

    m1 = mean(vec, na.rm = TRUE)

  }

  if ( missing(s1) ){

    s1 = sd(vec, na.rm = TRUE)

  }

  if ( missing(higherIsBetter)  ){

    a = m1 - 2*s1 # if higher is better, then the cutoff to obtain is larger

    return(a)

  } else if ( higherIsBetter == 0 ) {

    a = m1 - 2*s1  # if higher is not  better, then the cutoff to obtain is smaller

    return(a)

  } else if ( higherIsBetter == 1  ){

    a = m1 + 2*s1 # if higher is better, then the cutoff to obtain is larger

    return(a)

  }

}
#' @rdname jt_cutoffs
#' @usage jt_b(vec, m0, s1, higherIsBetter)
#' @export
jt_b <-  function(vec, m0, s1, higherIsBetter){


  if ( missing(s1) ){

    s1 = sd(vec, na.rm = TRUE)

  }

  if ( missing(higherIsBetter)  ){

    b = m0 + 2*s1 # if higher is not better, then the cutoff to obtain is approaching m0 from above

    return(b)

  } else if ( higherIsBetter == 0 ) {

    b = m1 + 2*s1  # if higher is not  better, then the cutoff to obtain is approaching m0 from above

    return(b)

  } else if ( higherIsBetter == 1  ){

    b = m0 - 2*s1 # if higher is better, then the cutoff to obtain is approaching m0 from below

    return(b)

  }

}
#' @rdname jt_cutoffs
#' @usage jt_c(m0, s0, m1, s1, equal.variance)
#' @export
jt_c <-  function(vec,
                  m0,
                  s0,
                  m1,
                  s1,
                  equal.variance = TRUE    # if set to true function will ignore s0, m1, and s1
){

  if (missing(m1)){
    m1 = mean(vec, na.rm = TRUE)
  }


  if (equal.variance == TRUE){

    c = (m0 + m1 ) / 2   # simply the halfway between the two

    return(c)

  } else if ((equal.variance == FALSE)){

    c = (s0*m0 + s1*m1) / (s0 +s1)

    return(c)

  }

}
