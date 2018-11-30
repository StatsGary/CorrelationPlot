######################COPYRIGHT GARY HUTSON################################
#######################Date 30th November 2018#############################

library(corrplot)
library(ggplot2)

#-----------CREATE FUNCTION------------------------------------------
create_gh_style_corrplot <- function(df_numeric_vals,
                                     method_corrplot,
                                     colour_min,
                                     colour_middle,
                                     colour_max="green") {
  
  library(corrplot)
  
  if(method_corrplot == 1 ){
    type_var <- "shade"
    method_corrplot = type_var  
    
  } 
  else if (method_corrplot ==2) {
    type_var <- "number"
    method_corrplot = type_var
  }
  
  else if (method_corrplot ==3) {
    type_var <- "pie"
    method_corrplot = type_var
  }
  else if (method_corrplot ==4) {
    type_var <- "ellipse"
    method_corrplot = type_var
  }
  else if (method_corrplot ==5) {
    type_var <- "circle"
    method_corrplot = type_var
  }
  else{
    type_var <- "shade"
    method_corrplot <- type_var
  }
  
  corrplot(cor(df_numeric_vals, use = 'all.obs'), method = method_corrplot, 
           order = "AOE",
           addCoef.col = 'black',
           number.cex = 0.5, 
           tl.cex = 0.6,
           tl.col = 'black',
           col= colorRampPalette(c(colour_min, colour_middle, colour_max))(200),
           cl.cex = 0.3)
}

##------------------CREATE DATASET---------------------------------------

numeric_df <- data.frame(mpg[c(3,5,8,9)])
#This relates to the numeric variables in the data frame to use with my function

##------------------USE FUNCTION-----------------------------------------

create_gh_style_corrplot(numeric_df,1, "steelblue2","white", "whitesmoke")
create_gh_style_corrplot(numeric_df,2, "steelblue2","black", "black")
create_gh_style_corrplot(numeric_df,3, "steelblue2","white", "whitesmoke")
create_gh_style_corrplot(numeric_df,4, "steelblue2","white", "whitesmoke")
create_gh_style_corrplot(numeric_df,5, "steelblue2","white", "whitesmoke")

