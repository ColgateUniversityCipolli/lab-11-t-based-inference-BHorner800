#######################################################
# Ben Horner
# Lab 11
# MATH 240
#######################################################
#######################################################
# Libraries
#######################################################
library(pwr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(xtable)
library(e1071)
library(effectsize)

#######################################################
# Power Analysis
#######################################################
pwr.t.test(d = 0.65, # moderate-large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample")

#######################################################
# Loading the data
#######################################################
#To collect the data, we downloaded all of their Figure 2 data, went to g, 
#then saved just the closer_vals and further_vals as a .csv file to easily use

finch.data = read.csv("./41586_2025_8729_MOESM4_ESM.csv") |>
  mutate(difference = Closer_vals - Farther_vals)

#Summarize the data:
finch.summary = summary(finch.data)
finch.summary

plot.finch = ggplot(finch.data, aes(x = difference)) + #assigns the data as the data frame and aes = x axis
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.1, fill = "lightblue", color = "black") + #makes histogram
  labs(title = "Difference between closer and further", #titles and axis labels
       x = "Dopamine Difference", 
       y = "Density")
plot.finch
#Suggests that further has negative dopamine change, closer has positive

#######################################################
# Conducting inferences from the paper
#######################################################
#Closer_vals
closer_t = t.test(x=finch.data$Closer_vals, alternative = "greater", mu = 0)
closer.t.stat = (closer_t[1]$statistic)
interpret_hedges_g(hedges_g(x = finch.data$Closer_vals, alternative = "greater", mu = 0))


#Farther_vals
farther_t = t.test(x=finch.data$Farther_vals, alternative = "less", mu = 0)
farther.t.stat = (farther_t[1]$statistic)
interpret_hedges_g(hedges_g(x = finch.data$Farther_vals, alternative = "less", mu = 0))

#Difference
difference_t = t.test(x=finch.data$difference, alternative = "two.sided", mu = 0) #two-sided
diff.t.stat = difference_t[1]$statistic
interpret_hedges_g(hedges_g(x = finch.data$difference, mu = 0)) #two-sided


#######################################################
# Plotting null T for each value
#######################################################
(n <- length(finch.data$Closer_vals))

# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))

#observed points
ggdat.closer <- tibble(t    =  closer.t.stat, 
                    y    = 0) # to plot on x-axis
ggdat.farther <- tibble(t    =  farther.t.stat, 
                       y    = 0) # to plot on x-axis
ggdat.difference <- tibble(t    =  diff.t.stat, 
                       y    = 0) # to plot on x-axis


ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null,
                color="T-distribution (Null)"))+
  geom_hline(yintercept=0) +
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot observation point
  geom_point(data=ggdat.closer, aes(x=t, y=y, 
             color="T-value closer")) +
  geom_point(data=ggdat.farther, aes(x=t, y=y, 
                                    color="T-value further")) +
  geom_point(data=ggdat.difference, aes(x=t, y=y, 
                                    color="T-value difference")) +
  theme_bw()+
  xlab("t")+
  ylab("Density")+
  labs(color="")
