#' @name jt.rci
#' @title Reliable Change Index
#' @description
#' Calculate the Reliable Change Index (RCI), and produce a corresponding plot in keeping with [Jacobson & Truax's 1991 article](https://pubmed.ncbi.nlm.nih.gov/2002127/). \cr
#'
#' While a p value tells us about the probability of observing a change of certain size, and a Standardised mean difference
#' (SMD ~ Cohen's d/Hedge's g) indicates
#' a probabilistic or statistical magnitude, a Reliable Change Index (RCI) discerns whether a change score measurement is a true positive or negative, according to; a pre-specified level of confidence,
#' the error of an instrument, and a normative distribution.
#'
#' \cr
#'
#' Extending the RCI to clinical significance suggests that changes in instrument scores larger than an RCI, would be perceptible
#' by an individual/patient, therapist, or significant other.
#' It is a common metric to estimate a threshold required to meet levels of clinical significance in psychometric studies, and
#' compatible with the concept of a Minimally Clinically Important Difference (MCID).
#'
#' \cr
#'
#' @param rxx The instrument test-retest reliability (typically ICC or Cronbach's alpha etc.)
#' @param wide.df A wide data frame, with pre and post instrument scores for each paired/matched observation
#' @param pre.vec Vector defining sample pre instrument scores
#' @param post.vec Vector defining sample post instrument scores
#'
#' @param sd_ref A reference/population standard deviation, if available, else the standard deviation of the pre-test scores is used.
#' @param NA_handling How should NAs be handled? Default is: `'ignore NAs'` - and calculates the SD of all pre-test scores available.  If set to `'use complete obs only'`, function will calculate the SD of observations from the set of pre-test scores for which there are also post-test scores. Note that patient data for only pre or post time points can be used to calculate RCI or be plotted on the chart, therefore this argument is only for specifying how to calculate the SD from the pre-test scores.
#'
#' @param Categories How should participants be grouped? Options are: `'cat.3'`, `'cat.5'`, `'JT.1'`, `'JT.2'`, `'JT.4'`.
#' @param unused_factor_drop Should unrepresented categorizations be dropped in plot and tabled output?
#'
#' @param min_to_use Minimum possible value of the instrument? If not provided the least positive/most negative value from both pre and post scores will be used.
#' @param max_to_use Maximum possible value of the instrument? If not provided the most positive value from both pre and post scores will be used.
#' @param higherIsBetter.num Are higher scores healthier? '1' - higher scores indicative of better health, '0' - higher scores indicative of worse health. (Defaults to '0' ).
#'
#' @param recovery_thrshld Should the recovery threshold information be included? If Categorisation requires the threshold, the recovery argument is ignored, and the threshold is plotted regardless.
#' @param thrshld_ref Insert a value for a threshold that defines *'case-ness'*. I.e. A threshold that is indicative of diagnosis, and not surpassing this threshold, a person would be deemed asymptomatic or recovered. If not supplied the function will impute the value as the healthiest quintile of the pre-test scores. Note that for `higherIsBetter.num = 0` (default) the lowest quintile is used. NA_handling also applies here and will tell the function whether to use all the pre-test scores (`'ignore NAs'`) or only those for which there are also post-test scores (`'use complete obs only'`)
#' @param thrshld_quantile An argument to specify which quantile to use to define recovery (defaults to `'5'`, i.e. quintiles).
#'
#' @param conf.lvl With what confidence level should false positives and negatives be screened? Default is `0.95`.
#'
#' @param rxx.cite `rxx` source?
#' @param sd_ref.cite `sd_ref` source?
#' @param thrshld_ref.cite `thrshld_ref` source?
#'
#' @param title ggplot `labs()` argument `title`
#' @param subtitle ggplot `labs()` argument `subtitle`
#' @param xlabel ggplot `labs()` argument `xlabel`
#' @param ylabel ggplot `labs()` argument `ylabel`
#' @param caption ggplot `labs()` argument `caption`
#'
#' @param colour_palette Provide a vector at least three (or 5 for larger classification) colours for chart. Otherwise defaults are used. This Will directly place the colour palette into the `ggplot()`. It is quick if only making a few charts. Use argument `colour.vec` if making many charts.
#' @param colour.vec Supply a vector of 5 colors. If using the three-way categorization (default), the second, fourth and fifth positions in the vector will be used. These reflect similar factor levels in differing categorisations. Recommended over `colour_palette`.
#'
#' @keywords   'RCI' 'RCC' 'psychometrics' 'MCID'
#' @return Returns a list including:
#'  \itemize{
#'  \item the RCI
#'  \item a ggplot using ~ `geom_point()`
#'  \item a data frame of proportions
#'  \item the the reference sd/sd used
#'  }
#'
#'  \cr
#'
#' @details The function `jt.rci()` manages psychometric scores at two time-points in line with Jacobson & Truax's foundational paper outlining the Reliable Change Index (RCI).
#' At a minimum, the function requires pre & post psychometric instrument scores from a paired or matched sample, and Jacobson and Truaux's value: `rxx` (usually ~ ICC or Cronbach's alpha).
#' If no argument is given for `sd_ref`, it is calculated from the pre-test scores.
#' The function will output a plot, an RCI, and a table detailing the proportions and numbers of individuals who may have improved, stayed the same, or deteriorated.
#'
#' The RCI is the real value of an instrument scale, with which we can say with a certain level of confidence (default is 95%),
#' that a clinically significant level of change has occurred. The RCI separates a sample of participants into those who have had truly positive improvement, deterioration, or those who who have had indeterminate change.
#' Therefore it is important to also report the percentages who reliably improved, stayed the same, and reliably deteriorated.
#' Participants who fall within 1.96 (`conf.lvl = 0.95`) RC of no change (these observations are located the diagonal bounds of the plots produced by the `jt.rci()` function) are considered false positives/false negatives.
#'
#' \cr
#'
#' @note
#'
#' \itemize{
#' \item Test/retest reliabilities are often included in validation studies. Try to use a reliability that was observed from a sample characteristically similar to the one you are investigating (Example: adults vs children )
#' \item For analyzing the RCI over multiple time points see [Morgan-Lopez et. al. (2022)](https://dx.doi.org/10.1002/mpr.1906)
#' }
#'
#' \cr
#'
#'
#' @source Refer to the article by Jacobson & Truax (1991) for a complete explanation:
#'
#' [Clinical significance: a statistical approach to defining meaningful change in psychotherapy research](https://pubmed.ncbi.nlm.nih.gov/2002127/) \cr
#'
#' \cr
#'
#' @seealso \itemize{
#' \item[Reliable change and the reliable change index: still useful after all these years?](https://dx.doi.org/10.1017/s1754470x22000484)
#' \item[Starting at the beginning: an introduction to coefficient alpha and internal consistency](https://doi.org/10.1207/S15327752JPA8001_18)
#' }
#'
#' \cr
#'
#' @example jt.rci_examples.R




# jt.rci --------------------------------------------------


#' @export
jt.rci <- function(

  rxx,                                # please define a value for rxx (numeric. example = 0.91)

  wide.df,                                   # a wide data frame, with two variables for pre and post instrument scores for each individual. each row should reflect the paired or matched data
  pre.vec,                                   # the variable for pre instrument scores for each individual
  post.vec,                                  # the variable for post instrument scores for each individual

  # ... The following are optional arguments

  sd_ref,                                 # please provide a reference/population standard deviation if available, other wise the sd of the pre-test scores will be used
  NA_handling = 'ignore NAs',                # default is : 'ignore NAs' - this will calculate the sd of all pre-test scores avaliable.  If set to 'use complete obs only', it will calculate the sd of observations from the set of pre-test scores for which there are also post-test scores. Note that patient data for only pre or post time points can be used to calculate RCI or be plotted on the chart, therefore this argument is only for specifying how to calculate the sd from the pre-test scores. If an sd_ref is specified this arguement is ignored with regards to sd_ref.
  Categories = 'cat.3',                            # should the output group individuals into three or five groups ? (default is three)
  unused_factor_drop = FALSE ,                # Should unrepresented catorgorisations be dropped in plot and tabled output?

  # plot coordinates

  min_to_use,                                # what is the minimum possible value of the instrument ? , if not provided the least positive/most negative value from both pre and post scores will be used
  max_to_use,                                # what is the maximum possible value of the instrument ? , if not provided the most positive value from both pre and post scores will be used
  # diretion
  higherIsBetter.num = 0,                    # are higher scores healthier? ('1' = YES - higher scores indicative of better health, '0' = NO - higher scores indicative of worse health  ( ~ defaults to '0' )

  # recovery info
  recovery_thrshld = FALSE,                  # should the recovery threshold by included? if Categories = 'cat.5', the recovery arguement is ignored, and the threshold is plotted regardless
  thrshld_ref,                            # insert a value for a threshold that defines 'case-ness'. Example: indicative for presence of diagnosis, and below this threshold, a person would be deemed asymptomatic or recovered. if not supplied the function will impute the value for the healthiest quintile of the pre-test scores. note that for higherIsBetter.num = 0 (default) the lowest quintile is used. NA_handling will tell the function whther to use all the pre-test scores (ignore NAs) or only those for which there are also post-test scores (use complete obs only)
  thrshld_quantile = 5,

  conf.lvl = 0.95,

  sd_ref.cite,                       # add a string to reference population sd use used (i.e. where was 'sd_ref' sourced from?), else the function will estimate from pre scores
  rxx.cite,                           # unless you provide a citation string the function will not cite.
  thrshld_ref.cite,                       # insert a citation for a threshold that defines a condition

  # provide your own chart labels

  title,
  subtitle,
  xlabel,
  ylabel,
  caption,                                   # If omitted, a default caption of the parameters used is calculated and used

  # colours

  colour_palette,                            # provide a vector at least three (or 5 for larger classification) colours for chart. Otherwise defaults are used. This Will directly place the colour palette into the ggplot. It is quick if only making a few charts. Use argument 'colour.vec' if making many charts
  colour.vec                                # Supply a vector of 5 colours. If using the three_way catorgorisation (default), the second, fourth and fifth positions in the vector will be used. These reflect identical factor levels in both categorisations. Recommended over colour_palette.



){

  # tells r to globally use the pipe operator from magrittr
  `%>%` <- magrittr::`%>%`


  #  first calculate a change_score

 df1 <- wide.df  %>%
   dplyr::ungroup() %>%
   dplyr::select( {{post.vec}}, {{pre.vec}} ) %>%
   dplyr::mutate(change_score = {{post.vec}} - {{pre.vec}} ) %>%
   dplyr::mutate(ID = dplyr::row_number())




  # default specification of sd (i.e. if sd is not provided by user)

  if ( missing(sd_ref) & NA_handling == 'use complete obs only'){

    sd_ref = sd( df1[ !is.na(df1$change_score) , 2] , na.rm = TRUE)

  } else if  ( missing(sd_ref) & (missing(NA_handling) | NA_handling == 'ignore NAs') ){

    sd_ref = sd( df1[ , 2], na.rm = TRUE)

  } else if ( !missing(sd_ref) ){

    sd_ref = sd_ref

  } else {

    print( "!! Forget to specify a reference Standard deviation? Or, spelling for NA_handling argument? Can be either 'ignore NAs', 'use complete obs only', or omitted (interpretted as 'ignore NAs') ")

  }


 # default sd citation

  if ( missing(sd_ref.cite) & NA_handling == 'use complete obs only'){

    sd_ref.cite = ', as estimated by the pre-test scores of completers'

    } else if  ( missing(sd_ref.cite) & (missing(NA_handling) | NA_handling == 'ignore NAs') ){

    sd_ref.cite = ', as estimated by pre-test scores'

    }



 # specify the z.val

 if( missing(conf.lvl)  ){  conf.lvl = 0.95  }

 z.val = qnorm(1 - ((1 - conf.lvl) / 2))





  # calculate RCI now

  rci = RCI( sdiff(SEm(sd_ref, rxx)) , conf.lvl )




  # default specification of min_to_use (i.e. if min_to_use is not provided by user)

  if ( missing(min_to_use) ){

    min_to_use = plyr::round_any( min(c( df1[ , 2], df1[ , 1] ), na.rm = TRUE) , 1,  floor)

  }

  # default specification of max_to_use (i.e. if max_to_use is not provided by user)

  if ( missing(max_to_use) ){

    max_to_use = plyr::round_any( max(c( df1[ , 2], df1[ , 1] ), na.rm = TRUE), 1,  ceiling)

  }

  # specify the range_to_use

  range_to_use = max_to_use - min_to_use




  # default specification of thrshld_ref (i.e. if thrshld_ref is not provided by user)

  if ( missing(thrshld_ref) & higherIsBetter.num == 0 & NA_handling == 'ignore NAs'){

    thrshld_ref =  round(quantile( df1[ , 2] , probs = seq(0, 1, 1/thrshld_quantile), na.rm = TRUE )[2], 1) # specify the cut off for condition among sample as the healthiest quintile.

  } else if (missing(thrshld_ref) & higherIsBetter.num == 1 & NA_handling == 'ignore NAs'){

    thrshld_ref =  round(quantile( df1[ , 2] , probs = seq(0, 1, 1/thrshld_quantile), na.rm = TRUE )[thrshld_quantile], 1) # specify the cut off for condition among sample as the healthiest quintile.

  } else   if ( missing(thrshld_ref) & higherIsBetter.num == 0 & NA_handling == 'use complete obs only'){

    thrshld_ref =  round(quantile( df1[ !is.na(df1$change_score) , 2] , probs = seq(0, 1, 1/thrshld_quantile), na.rm = TRUE )[2], 1) # specify the cut off for condition among sample as the healthiest quintile.

  } else if (missing(thrshld_ref) & higherIsBetter.num == 1 & NA_handling == 'use complete obs only'){

    thrshld_ref =  round(quantile( df1[ !is.na(df1$change_score) , 2] , probs = seq(0, 1, 1/thrshld_quantile), na.rm = TRUE )[thrshld_quantile], 1) # specify the cut off for condition among sample as the healthiest quintile.

  }






  # calculate the residuals and, use complete values only for plotting

  df2 <-  df1 %>%
    dplyr::mutate(rci_value = RC2({{pre.vec}}, {{post.vec}}, sd_ref, rxx)) %>%
    dplyr::filter(!is.na(change_score))



  # make a 3 way category classification for movement from pre to post

  df2$cat.3 <- c()

  if(higherIsBetter.num == 0){

    df2$cat.3 <- dplyr::if_else(df2$rci_value < -z.val, 'Reliably improved',
                       dplyr::if_else(df2$rci_value > z.val, 'Reliably deteriorated',
                               'Indeterminate change') )

  } else if(higherIsBetter.num == 1){

    df2$cat.3 <- dplyr::if_else(df2$rci_value > z.val, 'Reliably improved',
                       dplyr::if_else(df2$rci_value < -z.val, 'Reliably deteriorated',
                               'Indeterminate change') )

  }




  # make a 5 way cat classification for movement from pre to post

  df2$cat.5 <- df2$cat.3

  # df2$cat.5[df2$cat.3 == 'Reliably improved'] <- 'Reliably recovered'

  if(higherIsBetter.num == 0){

    df2 <- df2 %>%
      dplyr::mutate(cat.5 = dplyr::recode(cat.5, 'Reliably improved' = dplyr::if_else(  rci_value <  -z.val & {{post.vec}} <= (thrshld_ref - rci) , 'Reliably recovered', cat.5 ) ) ) %>%
      dplyr::mutate(cat.5 = dplyr::recode(cat.5, 'Indeterminate change' = dplyr::if_else(  rci_value >= -z.val & {{post.vec}} <= (thrshld_ref - rci) & df2$rci_value <= z.val, 'Unreliably recovered', cat.5 ) ) )

    # df2$cat.5[df2$rci_value < -z.val & df2$post.vec <= (thrshld_ref - rci) ] <- 'Reliably recovered'
    # df2$cat.5[df2$rci_value >= -z.val & df2$rci_value <= z.val & df2$post.vec <= thrshld_ref ] <- 'Unreliably recovered'

  } else if(higherIsBetter.num == 1) {

    df2 <- df2 %>%
      dplyr::mutate(cat.5 = dplyr::recode(cat.5, 'Reliably improved' = dplyr::if_else(  rci_value >  -z.val & {{post.vec}} >= (thrshld_ref + rci) , 'Reliably recovered', cat.5 ) ) ) %>%
      dplyr::mutate(cat.5 = dplyr::recode(cat.5, 'Indeterminate change' = dplyr::if_else(  rci_value >= -z.val & {{post.vec}} >= (thrshld_ref + rci) & df2$rci_value <= z.val, 'Unreliably recovered', cat.5 ) ) )

    # df2$cat.5[df2$rci_value >  z.val & df2$post.vec >= (thrshld_ref + rci) ] <- 'Reliably recovered'
    # df2$cat.5[df2$rci_value >= -z.val & df2$rci_value <= z.val & df2$post.vec >= thrshld_ref ] <- 'Unreliably recovered'

  }

    df2$cat.3 <-  df2$cat.3 %>%
    factor(levels = c('Reliably improved',
                      'Indeterminate change',
                      'Reliably deteriorated' ), ordered = TRUE)



    df2$cat.5 <-  df2$cat.5 %>%
    factor(levels = c('Reliably recovered',
                      'Reliably improved',
                      'Unreliably recovered',
                      'Indeterminate change',
                      'Reliably deteriorated' ), ordered = TRUE)




 # make two extra columns in df2 outlined by J&T called: 'Improved but not recovered',  and 'recovered'. These have Y or N values


    if(higherIsBetter.num == 0){

      df2 <- df2 %>%
        dplyr::mutate('Improved but not recovered' =  dplyr::if_else(  rci_value <  -z.val & {{post.vec}} >= (thrshld_ref) , 'Y', 'N' ) )

    } else if(higherIsBetter.num == 1) {

      df2 <- df2 %>%
        dplyr::mutate('Improved but not recovered' =  dplyr::if_else(  rci_value >  z.val & {{post.vec}} <= (thrshld_ref ) , 'Y', 'N' ) )
    }

 # 'Recovered'

    if(higherIsBetter.num == 0){

      df2 <- df2 %>%
        dplyr::mutate('Recovered' = dplyr::if_else( {{post.vec}} <= (thrshld_ref ) , 'Y', 'N' ) )


    } else if(higherIsBetter.num == 1) {

      df2 <- df2 %>%
        dplyr::mutate('Recovered' = dplyr::if_else( {{post.vec}} >= (thrshld_ref ) , 'Y', 'N' ) )
    }



    # describe the first three level categorisation J&T outline:


    if(higherIsBetter.num == 0){

      df2 <- df2 %>%
        dplyr::mutate('jt.1' = dplyr::if_else( {{post.vec}} < (thrshld_ref - rci ) , 'Recovered',
                                               dplyr::if_else( {{post.vec}} > (thrshld_ref + rci ), 'Unchanged or deteriorated' , 'Unclassifiable') )
        )


    } else if(higherIsBetter.num == 1) {

      df2 <- df2 %>%
        dplyr::mutate('jt.1' = dplyr::if_else( {{post.vec}} > (thrshld_ref + rci ) , 'Recovered',
                                               dplyr::if_else( {{post.vec}} < (thrshld_ref - rci ), 'Unchanged or deteriorated' , 'Unclassifiable') )
        )
    }


    df2$jt.1 <-  df2$jt.1 %>%
      factor(levels = c('Recovered',
                        'Unclassifiable',
                        'Unchanged or deteriorated' ), ordered = TRUE)


    # describe the second three level categorisation J&T outline:


    if(higherIsBetter.num == 0){

      df2 <- df2 %>%
        dplyr::mutate('jt.2' = dplyr::if_else( {{post.vec}} < (thrshld_ref - rci ) , 'Recovered',
                                               dplyr::if_else( rci_value < -z.val & {{post.vec}} > (thrshld_ref + rci ), 'Improved but not recovered' ,
                                                               dplyr::if_else( rci_value >= -z.val & {{post.vec}} > (thrshld_ref + rci ), 'Unchanged or deteriorated' , 'Unclassifiable' ) ) )
        )

    } else if(higherIsBetter.num == 1) {

      df2 <- df2 %>%
        dplyr::mutate('jt.2' = dplyr::if_else( {{post.vec}} > (thrshld_ref + rci ) , 'Recovered',
                                               dplyr::if_else( rci_value > z.val & {{post.vec}} < (thrshld_ref - rci ), 'Improved but not recovered' ,
                                                               dplyr::if_else( rci_value <= z.val & {{post.vec}} < (thrshld_ref - rci ), 'Unchanged or deteriorated' , 'Unclassifiable' ) ) )
        )

    }


    df2$jt.2 <-  df2$jt.2 %>%
      factor(levels = c('Recovered',
                        'Improved but not recovered',
                        'Unchanged or deteriorated' ), ordered = TRUE)








    # describe the fourth three level categorisation J&T outline:


    if(higherIsBetter.num == 0){

      df2 <- df2 %>%
        dplyr::mutate('jt.4' = dplyr::if_else( {{post.vec}} <= (thrshld_ref ) & rci_value < -z.val , 'Recovered',
                                               dplyr::if_else( rci_value < -z.val & {{post.vec}} > (thrshld_ref  ), 'Improved but not recovered' ,
                                                               dplyr::if_else( rci_value >= -z.val , 'Unchanged or deteriorated' , 'Uncertain' ) ) )
        )

    } else if(higherIsBetter.num == 1) {

      df2 <- df2 %>%
        dplyr::mutate('jt.4' = dplyr::if_else( {{post.vec}} >= (thrshld_ref ) & rci_value > z.val , 'Recovered',
                                               dplyr::if_else( rci_value > z.val & {{post.vec}} < (thrshld_ref  ), 'Improved but not recovered' ,
                                                               dplyr::if_else( rci_value <= z.val , 'Unchanged or deteriorated' , 'Uncertain' ) ) )
        )

    }


    df2$jt.4 <-  df2$jt.4 %>%
      factor(levels = c('Recovered',
                        'Improved but not recovered',
                        'Unchanged or deteriorated' ), ordered = TRUE)







    Cats.by.3 <- c( 'cat.3', 'jt.1', 'jt.2', 'JT.3', 'jt.4')
    Cats.by.5 <- c( 'cat.5')
    Cats.by.JT <- c( 'jt.1', 'jt.2', 'JT.3', 'jt.4')
    Cats.by.thrshld <-  c( 'cat.5', 'jt.1', 'jt.2', 'JT.3', 'jt.4')




  # builds the table and the chart labels for the plot

  # table

  test <- df2 %>%
    dplyr::group_by(
      if (Categories == 'cat.3'){
        cat.3
        } else if (Categories == 'cat.5'){
          cat.5
          } else if (Categories == 'jt.1'){
            jt.1
            } else if (Categories == 'jt.2'){
              jt.2
              } else if (Categories == 'jt.4'){
                jt.4
              },
      .drop = unused_factor_drop) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(!dplyr::if_any(dplyr::everything(), ~is.na(.x)  ) ) %>%  # this line is to drop NA's produced by the category jt.2
    dplyr::mutate(N = sum(n, na.rm = TRUE)) %>%
    dplyr::mutate(Outcomes = paste0( round( n/N*100 ,1) , '% (', n, '/', N ,')' )  )

  names(test)[1] <- 'label_text'

  test1 <- test %>%
    dplyr::mutate(plot_legend_labels = paste0( label_text, ' ' , n, '/', N , ' (', round( n/N*100 , 1) ,'%)' )  )

  test2 <- test %>%
    dplyr::select(-n, -N)

  table <- data.frame(t(test2[,-1]))
  colnames(table) <- dplyr::pull(test2[,1])



  plot_legend_labels <- test1$plot_legend_labels






  # default colours if not provided by user)

  if ( missing(colour.vec) ){
    colour.vec = c('#2471A3','#196F3D','#66b5a6','#8E44AD','#D35400')
  }

  colour.vec5 = colour.vec
  colour.vec3 = colour.vec5[c(2,4,5)]


  colour.df <- data.frame(text = c('Reliably recovered',
                                   'Reliably improved',
                                   'Unreliably recovered',
                                   'Indeterminate change',
                                   'Reliably deteriorated' ),

                          colour = colour.vec5,

                          cat.3.colour = c(NA),
                          cat.5.colour = c(NA),

                          text2 = c('Recovered',
                                    'bogus',
                                    'bogus',
                                    'Unclassifiable',
                                    'Unchanged or deteriorated' ),

                          text3 = c('Recovered',
                                    'bogus',
                                    'bogus',
                                    'Improved but not recovered',
                                    'Unchanged or deteriorated' ),

                          jt.1.colour = c(NA),
                          jt.2.colour = c(NA),
                          jt.4.colour = c(NA)



  )

  for (i in 1:5){

    colour.df[ i, 'cat.3.colour'  ] <-  colour.df$text[i] %in% df2$cat.3
    colour.df[ i, 'cat.5.colour' ] <-  colour.df$text[i] %in% df2$cat.5

    colour.df[ i, 'jt.1.colour' ] <-  colour.df$text2[i] %in% df2$jt.1
    colour.df[ i, 'jt.2.colour' ] <-  colour.df$text3[i] %in% df2$jt.2
    colour.df[ i, 'jt.4.colour' ] <-  colour.df$text3[i] %in% df2$jt.4
  }



  # default colours if not provided by user)

  if ( missing(colour_palette) & (Categories %in% Cats.by.3)  & unused_factor_drop == FALSE){

    colour_palette = colour.vec3

  }  else if ( missing(colour_palette) & Categories == 'cat.5' & unused_factor_drop == FALSE){

    colour_palette = colour.vec5

  } else if ( missing(colour_palette) & (Categories == 'cat.3') & unused_factor_drop == TRUE){

    colour_palette <- colour.df$colour[(colour.df$cat.3.colour == TRUE)]

  }  else if ( missing(colour_palette) & Categories == 'cat.5' & unused_factor_drop == TRUE){

    colour_palette <- colour.df$colour[(colour.df$cat.5.colour == TRUE)]

  } else if ( missing(colour_palette) & Categories == 'jt.1' & unused_factor_drop == TRUE){

    colour_palette <- colour.df$colour[(colour.df$jt.1.colour == TRUE)]

  } else if ( missing(colour_palette) & Categories == 'jt.2' & unused_factor_drop == TRUE){

    colour_palette <- colour.df$colour[(colour.df$jt.2.colour == TRUE)]

  } else if ( missing(colour_palette) & Categories == 'jt.4' & unused_factor_drop == TRUE){

    colour_palette <- colour.df$colour[(colour.df$jt.4.colour == TRUE)]

  }






  # chart parameters

  #--------------------------------- aesthetics jt_a line

  study_cite = '(Cohort derived)' # to label the line specified by the study

  jt_a_colour = 'blue'
  jt_a_type = "longdash"
  jt_a_width = 1
  jt_a_alpha = 0.6

  #---------------------------------------------  aesthetics thrshld line

  thrshld_colour = 'red'
  thrshld_type = "longdash"
  thrshld_width = 0.8
  thrshld_alpha = 0.5

  #--------------------------------------------- aesthetics midline (same score pre post)

  midline_colour = 'black'
  midline_type = "dashed"
  midline_width = 1
  midline_alpha = 0.3

  #--------------------------------------------- aesthetics diagonal RC lines

  diag_colour = 'black'
  diag_type = "solid"
  diag_width = 0.5
  diag_alpha = 0.4

  #---------------------------------------------  aesthetics citation (s) style

  cite_size = 3.2
  cite_style = 'italic'
  cite_alpha = 0.6
  cite_shift = 0.03

  #---------------------------------------------  aesthetics thrshld CI_box

  CI_box_alpha = 0.25
  CI_box_colour = 'darkgrey'






  #--------------------------------------- specify jt_a?

  jt_a = jt_a(df1[ , 2], higherIsBetter = higherIsBetter.num)

  #--------------------------------------------- x and y values for citation (s)



  thrshld_cite_x  = dplyr::if_else(higherIsBetter.num == 0, min_to_use, max_to_use )
  thrshld_cite_y = dplyr::if_else(higherIsBetter.num == 0, thrshld_ref - cite_shift*range_to_use, thrshld_ref + cite_shift*range_to_use)

  study_cite_x  = dplyr::if_else(higherIsBetter.num == 0, min(df1[ , 2], na.rm = TRUE), max(df1[ , 2], na.rm = TRUE) )
  study_cite_y = dplyr::if_else(higherIsBetter.num == 0, jt_a - cite_shift*range_to_use, jt_a + cite_shift*range_to_use)

  # #--------------------------------------------- CI_box



  CI_box <- data.frame(x = c(Inf, -Inf, -Inf, Inf),

                       y = c(thrshld_ref - rci ,
                             thrshld_ref - rci ,
                             thrshld_ref + rci ,
                             thrshld_ref + rci )
                       )

  #--------------------------------------------- visualise jt_plot chu9d week_6 md_ynger

  # Points falling above the dashed black diagonal mid-line  represent improvement,
  # points positioned on the dashed black diagonal mid-line  indicate no change,
  # and points below the dashed black diagonal mid-line indicate deterioration.

  if(higherIsBetter.num == 0){
    flip_vec <- c(  max_to_use, min_to_use  )
  } else if(higherIsBetter.num == 1){
    flip_vec <- c(  min_to_use, max_to_use  )
  }





  # default title

  if ( missing(title) ){

    title = 'Clinical Significance'

    }

  # default subtitle

  if ( missing(subtitle) ){

    subtitle = 'Scatter-plot of psychometric instrument pre and post scores'

  }

  # default xlabel

  if ( missing(xlabel) ){

    xlabel = 'Pre-test Scores'

  }

  # default ylabel

  if ( missing(ylabel) ){

    ylabel = 'Post-test Scores'

  }



  # default thrshld_ref.cite

  if ( missing(thrshld_ref.cite) ){

    thrshld_ref.cite = '(Cohort derived)'

  }





  # default caption

  if (missing(caption) & recovery_thrshld == FALSE & missing(thrshld_ref) ){

     caption = paste0( '\nRCI ( +/- ', signif(rci, 2) ,') calculated using the test reliability coefficient ', rxx , (if (missing(rxx.cite) ){} else { paste0( ' ', rxx.cite ) }  ), '\nStandard error of measurement calculated using SD = ', round(sd_ref, 1) , sd_ref.cite )

  } else if (missing(caption) & ( recovery_thrshld == TRUE | !missing(thrshld_ref) | Categories == 'cat.5' | Categories == 'jt.1' | Categories == 'jt.2' | Categories == 'jt.4' ) ){

    caption = paste0('Threshold of recovery specified as', if (higherIsBetter.num == 1){ ' >= '} else if (higherIsBetter.num == 0){ ' <= '}, thrshld_ref , ' ', thrshld_ref.cite , '\nRCI ( +/- ', signif(rci, 2) ,') calculated using the test reliability coefficient ', rxx , (if (missing(rxx.cite) ){} else { paste0(' ', rxx.cite ) }  ), '\nStandard error of measurement calculated using SD = ', round(sd_ref, 1) , sd_ref.cite )

  }








  # plot

  plot1 <- ggplot2::ggplot() +

    # # add a shaded confidence interval given the RCI calulated
    # geom_polygon(aes(x = x, y = y), data = CI_box, fill = CI_box_colour, alpha = CI_box_alpha) +

    ggplot2::geom_point( data = df2, ggplot2::aes( x = {{pre.vec}},
                                                   y = {{post.vec}},
                                                   colour =   if (Categories == 'cat.3'){

                                                     cat.3

                                                     } else if (Categories == 'jt.1'){

                                                       jt.1

                                                     } else if (Categories == 'jt.2'){

                                                       jt.2

                                                     } else if (Categories == 'jt.4'){

                                                       jt.4

                                                     } else if (Categories == 'cat.5'){

                                                       cat.5

                                                       }
                                                   ,

    )

    ) +

    # colours for the observations
    ggplot2::scale_colour_manual(labels = plot_legend_labels, values = colour_palette , drop = unused_factor_drop
                        # labels = category_labs
    ) +

    # adds a diagonal line 'midline'
    ggplot2::geom_abline(slope = 1, intercept = 0,  colour = midline_colour, linetype = midline_type, linewidth = midline_width, alpha = midline_alpha) +

    # adds the boundary lines specified by the RCI
    ggplot2::geom_abline(slope = 1, intercept = rci, colour = diag_colour, linetype = diag_type, linewidth = diag_width, alpha = diag_alpha) +
    ggplot2::geom_abline(slope = 1, intercept = -rci, colour = diag_colour, linetype = diag_type, linewidth = diag_width, alpha = diag_alpha) +

    # xlim( flip_vec) +
    # ylim( flip_vec) +

    ggplot2::scale_x_continuous(

      expand = ggplot2::expansion(mult = .05),
      # limits = flip_vec
      # ,
      transform = if (higherIsBetter.num == 1){"identity"} else if (higherIsBetter.num == 0){"reverse"}
                        ) +
    ggplot2::scale_y_continuous(

      expand = ggplot2::expansion(mult = .05),
      # limits = flip_vec
      # ,
      transform = if (higherIsBetter.num == 1){"identity"} else if (higherIsBetter.num == 0){"reverse"}
                        ) +

    ggplot2::coord_cartesian(
      xlim = flip_vec,
      ylim = flip_vec,
    ) +

    ggplot2::labs(title = paste0(  title ),
                  subtitle = paste0( subtitle),
                  x = paste0( xlabel ),
                  y = paste0( ylabel),
                  caption = paste0(caption),
                  colour = 'Category')


  plot2 <- plot1 +
      # geom_area()

    # add a shaded confidence interval given the RCI calulated
    ggplot2::geom_polygon(ggplot2::aes(x = x, y = y), data = CI_box, fill = CI_box_colour, alpha = CI_box_alpha) +

    # adds the threshold line specified by literature
    ggplot2::geom_hline( yintercept = thrshld_ref, colour = thrshld_colour, linetype = thrshld_type, linewidth = thrshld_width, alpha = thrshld_alpha) +
    # adds the citation specified by thrshld_chu9d_cite
    ggplot2::annotate(geom = 'text', x = thrshld_cite_x, y = thrshld_cite_y, hjust = 1, label = paste0(thrshld_ref.cite, ' (', thrshld_ref , ')' ) , size = cite_size, fontface = cite_style, alpha = cite_alpha)

    # # adds the threshold line specified by jacobson & truax a) estimated by the pre-test scores
    # geom_hline( yintercept = jt_a, colour = jt_a_colour, linetype = jt_a_type, linewidth = jt_a_width, alpha = jt_a_alpha) +
    # # adds the citation specified by the study
    # annotate(geom = 'text', x = study_cite_x, y = study_cite_y, hjust = 1, label = study_cite , size = cite_size, fontface = cite_style, alpha = cite_alpha) +




 if (recovery_thrshld == FALSE & Categories != 'cat.5' & Categories != 'jt.1' & Categories != 'jt.2' & Categories != 'jt.4' ){
   plot = plot1
 } else if (recovery_thrshld == TRUE  | Categories == 'cat.5' | Categories == 'jt.1' | Categories == 'jt.2' | Categories == 'jt.4' ){
   plot = plot2
 }



  # output

  out <- list(rci = rci,
              plot = plot,
              table = table,
              sd_used = sd_ref

              # ,RC.vals = df2
              )

  return(out)



}










# the true score  -----------------------------------

#' @export
comp.true <- function(rxx, x, m0, higherIsBetter){

  if(missing(m0)){

    m0 = mean(x, na.rm = TRUE)

  }

  if ( missing(higherIsBetter) ){

    ( rxx*(x) - (1 - rxx)*m0  )

  }  else if ( higherIsBetter == 1 ){

    ( rxx*(x) + (1 - rxx)*m0  )

  }  else if ( higherIsBetter == 0 ){

    (  rxx*(x) - (1 - rxx)*m0  )

  }


}






# # the RCI sub functions
#
# # RCI ----------------------------------------------------
#
# SEm <- function(sd_norm, rxx){ sd_norm*sqrt(1 - rxx)} #standard error of measurement (In Jacobson & Truax 1991 this is know as SE)
#
# # for SEm - sd_norm is the Standard deviation of a normal population, or control group, rxx is the test- retest reliability, or internal consistency (Cronbach's alpha)
#
# sdiff <- function(SEm){sqrt(2*SEm^2)} # standard error of the difference between pre and post scores (describes the spread of the distribution (sd) of change scores, that would be expected if no actual change had occurred. An RC larger than 1 .96 would be unlikely to occur (p < .05) without actual change.)
#
# RC <- function(pre, post, sdiff){ (post - pre) / sdiff } # RC is reminiscent of Cohen's d but not the same. The denominators for both d and RC are spreads of data. For d it is of the change distribution, whereas for RC it is of the distribution for which no change has occurred. If RC is greater than z.val (usu 1.96), it is a rare observation - an unlikely large change.
#
# RC2 <- function(pre, post, sd_norm, rxx){ (post - pre) / sqrt(2*(sd_norm*sqrt(1 - rxx))^2) } # RC2 is the same as RC, but takes the rxx, and sd_norm, rather than the computed sdiff
#
# RCI <- function(sdiff){sdiff*1.96}







