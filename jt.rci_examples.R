

# Examples ----

library(tidyverse)

# using the sample data in `rci.data` dataframe run the following code.
# Execution will produce some values, an RCI, a plot, a table, and the sd used (also printed in default caption)
# The function should print a list in the console, including an RCI (first object), a plot (second object - in the 'Plots' tab), the table (third object) of instances with proportions of individuals in each group, and the sd used (fourth object)

jt.rci(

 0.88,
 rci.data,
 pre,
 post

)



## Calling individual outputs: ----

#  in this code the output ( a list) is placed into an object (my_jt.rci_object) and saved in the  work environment,

my_jt.rci_object <- jt.rci(

  0.88,

  rci.data,
  pre,
  post

)

# access the individual outputs using the following:

my_jt.rci_object$rci   # the estimated RCI for this data
my_jt.rci_object$plot   # plot
my_jt.rci_object$table   # tabled results
my_jt.rci_object$sd_used   # sd used

# or alternatively...

my_jt.rci_object[[1]]   # the estimated RCI for this data
my_jt.rci_object[[2]]   # plot
my_jt.rci_object[[3]]   # tabled results
# ...



# the RCI is numeric i.e: it can be treated as a number

my_jt.rci_object[[1]]*15                   # multiply
round( my_jt.rci_object[[1]] , 2)          # rounding decimals

# the plot is a ggplot object i.e: it can be treated as a ggplot2 object and exported as such

my_jt.rci_object[[2]] + ggplot2::theme_minimal()
my_jt.rci_object[[2]] + ggplot2::theme(legend.position = "top")
my_jt.rci_object[[2]] + ggplot2::labs(caption = "")

# the tabled results can be inspected as a dataframe:

table_df <- my_jt.rci_object[[3]]         # save the table (as dataframe)
View(table_df)                            # view the dataframe

# change the order of the columns

rearranged_table_df <- table_df %>% select(c(1,3,2), everything())
View(rearranged_table_df)



# Other options: ----

##  NA_handling ----

# in the first example, the SD used to calculate the output was printed in the caption of the plot and printed in the bottom of the screen
# it can also be retrieved:

my_jt.rci_object$sd_used

# Users can tell the function to only use pre.vec-test scores for which there are corresponding post-test scores by setting the argument 'NA_handling' = 'use complete obs only'
# The sd_used should have changed slightly in these two examples, as the pre and post scores in rci.data contain missing values

jt.rci(

  0.88,
  rci.data,
  pre,
  post,
  NA_handling = 'use complete obs only',

)



## Recovery threshold: ----

# display the recovery threshold using  recovery_thrshld = TRUE,

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  recovery_thrshld = TRUE

)

# Alternatively, using Categories = 'cat.5' (or other arguments that depend on the recovery threshold) will ignore recovery_thrshld argument and include the threshold as mandatory

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  Categories = 'cat.5',
  recovery_thrshld = FALSE     # ignores this argument

)



## Supply a threshold of recovery: ----

# supply a threshold from the literature

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  Categories = 'cat.3',
  thrshld_ref = 8,
  recovery_thrshld = TRUE,
  higherIsBetter.num = 1

)



## higherIsBetter.num: ----

# If higher scores are indicative of better health higherIsBetter.num should be switched to '1'

jt.rci(

  0.98,
  rci.data,
  pre,
  post,
  Categories = 'cat.5',
  higherIsBetter.num = 1

)


## Unrepresented levels: ----

# If a poor/low rxx is selected, it is unlikely that individuals will fall into the 'Unreliably Recovered' category  (5 levels). If so there will be no colour placed next to this level in the chart, as it is not plotted in the plot area.
# If it is preferred that these levels are not reported (though not recommended), user can set 'unused_factor_drop' to 'TRUE'

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  Categories = 'cat.5',
  higherIsBetter.num = 1 ,
  unused_factor_drop = TRUE

)



## Externally supplied standard deviation: ----

jt.rci(

  0.88,
  rci.data,
  pre,
  post,
  sd_ref = 8.902,
  sd_ref.cite = '(My Reliable Sauce)',

)



## Citations: ----

jt.rci(

  0.88,

  rci.data,
  pre,
  post,
  sd_ref.cite = ' - My Reliable Sauce',
  rxx.cite = ' (Scooby & Doo (2025))',
  thrshld_ref.cite = '(~ a Validation Study)',
  recovery_thrshld = TRUE
)



## Plot area: ----

# It's usually best to place the full range of the psychometric instrument on the plot
# Specify these using the min_to_use and max_to_use arguments
# For an example suppose the true maximums and minimums of a psychometric scale are defined by minima and maxima:

minima = plyr::round_any( min(c( rci.data[ , 2], rci.data[ , 3] ), na.rm = TRUE) - 5 , 1,  floor) # add padding (5) to the lower limit
maxima = plyr::round_any( max(c( rci.data[ , 2], rci.data[ , 3] ), na.rm = TRUE) + 5 , 1,  ceiling) # add padding (5) to the upper limit


jt.rci(

  0.88,
  rci.data,
  pre,
  post,
  min_to_use =  minima,
  max_to_use = maxima,

)



## Colours: ----

# Using the colour_palette argument you can intuitively place the colours in a vector to match the displayed output
# Provide exactly the colours you prefer in a vector

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  Categories = 'cat.3',
  colour_palette = c('blue', 'darkgreen', 'red'),

)

# Note that if not enough colours are provided errors ensue:
# "! Insufficient values in manual scale. 5 needed but only 3 provided"

jt.rci(

  0.78,
  rci.data,
  pre,
  post,
  Categories = 'cat.5',
  colour_palette = c('blue', 'darkgreen', 'red'),
)


# the colour.vec is a better way to choose preferred colours and will reflect categorisations consistently over many trials
# Note that'colour_palette' is a direct value placed in a ggplot and will override colour.vec if both are specified in the function.

jt.rci(

  0.70,
  rci.data,
  pre,
  post,
  Categories = 'cat.5',
  colour.vec = c('blue', 'darkgreen', 'red', 'yellow', 'brown'),
  unused_factor_drop = TRUE

)



## Plot labels: ----

jt.rci(

  0.88,
  rci.data,
  pre,
  post,

  title = 'My title',
  subtitle = 'Pre-post scores of Kessler-10 among adults',
  caption = paste0('Sample size = ', dim(rci.data)[1]),
  xlabel = 'Pre K-10',
  ylabel = 'Post K-10'

)



# JT Examples ----

# DAS example

thrshld_ref.DAS = round(jt_a(jt.data$DAS.Pretest, higherIsBetter = 1),1)

jt.rci(rxx = 0.96,
       jt.data,
       DAS.Pretest,
       DAS.Posttest,
       sd_ref = 17.8,
       min_to_use = 40,
       max_to_use = 140,
       higherIsBetter.num = 1,
       thrshld_ref = thrshld_ref.DAS,
       thrshld_ref.cite = paste0(" (Jacobson & Truax\'s value: 'a')" ),
       sd_ref.cite = ' (Spanier, 1976)' ,
       rxx.cite = ' (Spanier, 1976)',
       Categories = 'jt.4'
)


# GDS example

thrshld_ref.GDS = round(jt_c(jt.data$GDS.Pretest, m0 = 50), 1)

jt.rci(rxx = 0.75,
       jt.data,
       GDS.Pretest,
       GDS.Posttest,
       higherIsBetter.num = 0,
       thrshld_ref = thrshld_ref.GDS,
       thrshld_ref.cite = paste0(" (Jacobson & Truax\'s value: 'c')" ),
       rxx.cite = ' (average correlation of MSI subscales - Snyder, 1976)',
       Categories = 'jt.4'
)




