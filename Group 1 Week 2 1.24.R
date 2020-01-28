####################################################################################
#  File 1 for Week 1
#  Group 1 Assignment 1
#  Team Members: Maggie Dolan, Asheley Faris, Megan Jones, Tina Ladson, Daniel Ward
#  Jan 17, 2020 
#  MBA 8045
####################################################################################

##########################################################Install Packages###########################################

install.packages("pacman")
pacman::p_load(dplyr,tidyr,plyr,ggplot2,hrbrthemes,ggcorrplot,readr,reshape,stringi,gmodels,magrittr,pdftools,corrgram,Hmisc,BiocManager,rms,janitor)

################################################################Read in a data set################################################ 

Spotify <- read_csv("C:\\Users\\mjone\\OneDrive\\Desktop\\MBA 8045\\Data\\top10s.csv")

Spotify %>%
  clean_names()

Spotify <- Spotify %>% 
  dplyr::rename(
    rank = X1)

head(Spotify)

sink("C:\\Users\\mjone\\OneDrive\\Desktop\\MBA 8045\\SR\\week2.txt")
summary(Spotify)
simple.fit = lm(pop~dnce, data=Spotify)
summary(simple.fit)
multi.fit = lm(pop~bpm+dnce, data=Spotify)
summary(multi.fit)
sink()

###############################################################For Loop For Regression Analysis##################################
output <- vector("double", ncol(Spotify))  # 1. output
for (i in Spotify)             # 2. sequence
  {output[[i]] <- simple.fit = lm(pop~(Spotify[[i]], data=Spotify))
                                 summary(simple.fit)  }#3. Body


################################################################Plotting###########################################################
pdf("C:\\Users\\mjone\\OneDrive\\Desktop\\MBA 8045\\Images\\Week2.pdf")

corrgram(Spotify, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Top Spotify Songs 2010-2019")

# Create scatterplot
plot(x = Spotify$val,        # X coordinates
     y = Spotify$pop,        # y-coordinates
     main = 'Top Spotify Songs 2010-2019!',
     xlab = 'Dancebility',   # x-axis label
     ylab = 'Popularity',   # y-axis label
     pch = 16,                  # Filled circles
     col = gray(.0, .1))        # Transparent gray

grid()        # Add gridlines

# Create a linear regression model
model <- lm(formula = dnce ~ pop, 
            data = Spotify)

abline(model, col = 'blue')  # Add regression to plot


ggplot(Spotify, aes(val,pop) +
  geom_point(aes(color = artist)) +
  geom_smooth(se = FALSE) +
  labs(title = "Dancebility Pop")

  

dev.off()

p_unload(all)

