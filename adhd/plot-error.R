library(readr)
library(dplyr)
library(ggplot2)
library(grid)
# library(gridExtra)

file_path <- "reduced_adhd.csv"

df <- read_csv(file_path)
# df %>% View

# NB: This apparently includes the summarySE function but it's not available for the version
# of R that I'm using...
install.packages("bear")

... so load this instead.

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

new_df <- summarySE(df, measurevar = "err", groupvars = c("size", "cv"))
# new_df %>% View


pd <- position_dodge(0.35)
p1 <- ggplot(new_df, aes(x = as.factor(size), y = err, colour = cv)) +
  geom_point(size = 2, position = pd) + 
  geom_errorbar(aes(ymin = err - se, ymax = err + se), position = pd, width = 0) +
  theme_bw() +
  xlab("Size of Training Set") +
  ylab("Mean Squared Error")

ggsave("error-plot.pdf", p1, width = 4.5, height = 3.5, units = "in")

########################
# Difference in Errors #
########################

diff_df <- read_csv("adhd/adhd_dif.csv")

new_diff_df <- summarySE(diff_df, measurevar = "diff", groupvars = "size")
# new_diff_df %>% View


pd <- position_dodge(0.35)
p2 <- ggplot(new_diff_df, aes(x = as.factor(size), y = diff)) +
  geom_point(size = 2, position = pd) + 
  geom_errorbar(aes(ymin = diff - se, ymax = diff + se), width = 0) +
  theme_bw() +
  xlab("Size of Training Set") +
  ylab("Mean of Difference in Squared Error")

ggsave("difference-error-plot.pdf", p2, width = 4.5, height = 3.5, units = "in")

# ggpairs(p1, p2, ncol = 2)

# Define grid layout to locate plots and print each graph
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
