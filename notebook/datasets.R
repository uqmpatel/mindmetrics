

 set.seed(321)
 sample.size = 160

  rci.data <-  data.frame(
    ID = c(paste0( 'ID', stringr::str_pad( (seq(1, sample.size, by = 1) ), 4, 'left', '0' ) ) ),
    pre = replace(round(abs(rnorm(sample.size, 29, 12.2)), 0 ) , round(runif(n = 19, min = 1, max = sample.size), 0),  NA  )   ,
    post = replace(round(abs(rnorm(sample.size, 22, 8.3)), 0 ) , round(runif(n = 28, min = 1, max = sample.size), 0),  NA  )  )


# save(rci.data, file = "data/rci.data.rda")
  usethis::use_data(rci.data, overwrite = TRUE)




DAS.data <- data.frame( subject = seq(1,30, by = 1),
                        DAS.Pretest =  c(90.5, 74.0,  97.0, 73.5, 61.0, 66.5, 68.5,  86.5,  88.5, 68.5, 98.0,  80.5, 89.5,  91.5, 83.5, 60.5, 83.0,  88.0,  98.5,  78.5,   99.5,  79.5,  84.5,  92.5,  93.0, 85.0,  64.0, 61.0, 80.0, 82.5),
                        DAS.Posttest = c(97.0, 124.0, 97.5, 88.0, 96.5, 62.5, 112.5, 103.5, 90.0, 82.5, 105.0, 99.5, 112.5, 101.0, 99.5, 79.5, 88.0, 100.5, 119.0, 116.0,  116.0, 129.0, 113.0, 118.0, 92.0, 114.0, 68.0, 52.0, 60.5, 104.5))

GDS.data <- data.frame( subject = seq(1,30, by = 1),
                        GDS.Pretest =  c(68.0, 74.5, 58.5, 73.5, 78.5, 76.0, 76.5, 63.0, 70.0, 75.0, 63.5, 73.5, 71.5, 63.5, 57.0, 75.0, 63.0, 75.0, 71.5, 68.0, 75.5, 67.5, 62.5, 69.5, 61.0, 67.0, 75.5, 75.5, 69.5, 66.5),
                        GDS.Posttest = c(62.5, 56.0, 58.0, 71.0, 60.5, 77.0, 58.5, 52.0, 65.5, 73.0, 64.0, 55.5, 53.0, 55.0, 50.0, 78.0, 65.5, 62.0, 60.5, 51.0, 50.0, 44.0, 55.5, 56.0, 60.5, 47.5, NA, NA, NA , NA))

jt.data <- merge(DAS.data, GDS.data, by = 'subject', all = TRUE)
#
#
# save(jt.data, file = "data/jt.data.rda")
usethis::use_data(jt.data, overwrite = TRUE)

devtools::document()
devtools::install()




usethis::use_git_config(user.name = "uqmpatel", user.email = "152931345+uqmpatel@users.noreply.github.com")

usethis::use_git()
usethis::use_github()



#'
#' #' @description
#' Calculate the cutoffs (a, b, and c) in keeping with Jacobson & Truax's 1991 original article. \cr
#'
#'
#' #' @keywords 'threshold', 'Jacobson and Truax'
#' @return A numerical value
#'
#'
#' @source Refer to the article by Jacobson, N. S. & Truax, P. for a complete explanation:
#'
#' [Clinical significance: a statistical approach to defining meaningful change in psychotherapy research](https://pubmed.ncbi.nlm.nih.gov/2002127/)
#'
#'
#' @seealso \itemize{
#' \item[Reliable change and the reliable change index: still useful after all these years?](https://dx.doi.org/10.1017/s1754470x22000484)
#' \item[Starting at the beginning: an introduction to coefficient alpha and internal consistency](https://doi.org/10.1207/S15327752JPA8001_18)
#' \item[](https://doi.org/10.1016/0005-7967(89)90076-4)
#' }
#'
#' @example jt.threshold_examples.R

