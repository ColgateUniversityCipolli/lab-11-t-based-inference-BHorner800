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

###############
#Plotting Closer:
###############
# Resampling to approximate the sampling distribution 
# on the data
x = finch.data$Closer_vals
mu0 = 0
s <- sd(x)
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              closer.t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0


closer_plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0) +
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # plot observation point
  geom_point(data=ggdat.closer, aes(x=t, y=y),
             color="red") +
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     labels = round(t.breaks, 2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Dopamine Closer to Adult Song")


###############
#Plotting Further:
###############
# Resampling to approximate the sampling distribution 
# on the data
x = finch.data$Farther_vals
mu0 = 0
s <- sd(x)
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              farther.t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0


farther_plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0) +
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # plot observation point
  geom_point(data=ggdat.farther, aes(x=t, y=y), 
                                    color="red") +
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     labels = round(t.breaks, 2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Dopamine Further from Adult Song")

###############
#Plotting Further:
###############
# Resampling to approximate the sampling distribution 
# on the data
x = finch.data$difference
mu0 = 0
s <- sd(x)
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=x,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              diff.t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0


difference_plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0) +
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # plot observation point
  geom_point(data=ggdat.difference, aes(x=t, y=y), 
                                     color="red") +
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     labels = round(t.breaks, 2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Difference in Dopamine")

(closer_plot / farther_plot)+ difference_plot

