---
title: "drugs&driving"
author: "Jacob Koppel Egierd"
date: "May 11, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

# Import "ddimpff""

library(readr)
ddimpff <- read_csv("/home/jacob/Documents/ddimpff.csv", 
    col_types = cols(X11 = col_skip(), X12 = col_skip(), 
        X17 = col_skip(), X18 = col_skip(), 
        alc08 = col_skip(), alc16 = col_skip(), 
        alc_crash = col_number(), alc_pm = col_number(), 
        cig_pm = col_number(), drg08 = col_skip(), 
        drg16 = col_skip(), drg_crash = col_number(), 
        ill_py = col_number(), leg_stat = col_character(), 
        mj_pm = col_number()))
View(ddimpff)

# Import "fca"

library(readr)
fca <- read_csv("/home/jacob/Documents/fca.csv", col_types = cols(abrev = col_character(), 
    alc08 = col_number(), alc16 = col_number(), 
    alc_change = col_number(), drg08 = col_number(), 
    drg16 = col_number(), drg_change = col_number()))
View(fca)

# Import "dimpfff" (this one has report rates included)

ddimpfff <- read_csv("/home/jacob/Documents/ddimpfff.csv", 
    col_types = cols(X11 = col_skip(), X12 = col_skip(), 
        X17 = col_skip(), X18 = col_skip(), 
        alc08 = col_skip(), alc16 = col_skip(), 
        alc_crash = col_number(), alc_pm = col_number(), alc_reprate = col_number(),
        cig_pm = col_number(), drg08 = col_skip(), 
        drg16 = col_skip(), drg_crash = col_number(), drg_reprate = col_number(),
        ill_py = col_number(), leg_stat = col_character(), 
        mj_pm = col_number())) 
View(ddimpfff)

# Import data containing "plot_CI" function

download.file('http://www.rci.rutgers.edu/~mmm431/psych_400/data/mlb11.RData',
              destfile='~/R/data/mlb11.RData')
load("~/R/data/mlb11.RData")

```

## R Markdown

Data Analysis I

```{r}

# Create new column classifying states wherein marijuana is legal and all other states as not
ddimpff$leg_stat_boolio <- with(ddimpff, ifelse(leg_stat == "legal", "legal", "not"))

# Create initial subsets

stat_all_drg_08_16 <- subset(ddimpff, drg_crash > 0)
stat_all_drg_08 <- subset(ddimpff, drg_crash > 0 & yr=="2008")
stat_all_drg_16 <- subset(ddimpff, drg_crash > 0 & yr=="2016")
stat_all_alc_08_16 <- subset(ddimpff, alc_crash > 0)
stat_all_alc_08 <- subset(ddimpff, alc_crash > 0 & yr=="2008")
stat_all_alc_16 <- subset(ddimpff, alc_crash > 0 & yr=="2016")
stat_all_08_16 <- subset(ddimpff, alc_crash > 0 & drg_crash > 0)

# Subset only data for states wherein marijuana is strictly legal, medicinal, or illegal (excluding decriminalized status)
leg_n_no_slash_med_tdata_new <- subset(ddimpff, drg_crash > 0.0 & leg_stat=="legal" | drg_crash > 0.0 & leg_stat=="med" | drg_crash > 0.0 & leg_stat=="no")

# Test significance of legal status on drug involved crashes
t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_slash_med_tdata_new, var.equal=TRUE)

# Subset only data for states wherein marijuana is strictly legal or illegal (excluding decriminalized and medicinal status)
leg_n_no_tdata_new <- subset(ddimpff, drg_crash > 0.0 & leg_stat=="legal" | drg_crash > 0.0 & leg_stat=="no")

# Test significance
t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_tdata_new, var.equal=TRUE)
t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_slash_med_tdata_new, var.equal=TRUE)

# Subset data in the same manner, but only for 2016

leg_n_no_tdata_new_2016 <- subset(ddimpff, drg_crash > 0.0 & leg_stat=="legal" & yr=="2016" | drg_crash > 0.0 & leg_stat=="no" & yr=="2016")
leg_n_no_slash_med_tdata_new_2016 <- subset(ddimpff, drg_crash > 0.0 & leg_stat=="legal" & yr=="2016" | drg_crash > 0.0 & leg_stat=="med" & yr=="2016" | drg_crash > 0.0 & leg_stat=="no" & yr=="2016")

# Test significance

t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_tdata_new_2016, var.equal=TRUE)
t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_slash_med_tdata_new_2016, var.equal=TRUE)

# Load ggplot2

library("ggplot2")

# Visualize relationships with boxplot

fca_plot_drg_crash <- ggplot(leg_n_no_tdata_new,aes(x=leg_stat_boolio,y=drg_crash)) + geom_boxplot()
leg_plot_drg_crash <- fca_plot_drg_crash + geom_point(alpha=0.2)
leg_plot2_drg_crash <- leg_plot_drg_crash + stat_summary(fun.y="mean",geom="point", size=4,color="red3")
print(leg_plot2_drg_crash)

# Visualize differences between all legalities

fca_plot_drg_crash_all <- ggplot(stat_all_drg_08_16,aes(x=leg_stat,y=drg_crash)) + geom_boxplot()
leg_plot_drg_crash_all <- fca_plot_drg_crash_all + geom_point(alpha=0.2)
leg_plot2_drg_crash_all <- leg_plot_drg_crash_all + stat_summary(fun.y="mean",geom="point", size=4,color="red3")
leg_plot2_drg_crash_all + facet_grid(~yr)

# Explore relationship of legality on alcohol crash rates

fca_plot_alc_crash_all <- ggplot(stat_all_alc_08_16,aes(x=leg_stat,y=alc_crash)) + geom_boxplot()
leg_plot_alc_crash_all <- fca_plot_alc_crash_all + geom_point(alpha=0.2)
leg_plot2_alc_crash_all <- leg_plot_alc_crash_all + stat_summary(fun.y="mean",geom="point", size=4,color="red3")

# Test significance of legal status on alcohol involved crashes
t.test(alc_crash ~ leg_stat_boolio, data=leg_n_no_tdata_new, var.equal=TRUE)
t.test(alc_crash ~ leg_stat_boolio, data=leg_n_no_slash_med_tdata_new, var.equal=TRUE)

# Visualize change in marijuana use

fca_plot_mj_pm_all <- ggplot(stat_all_08_16,aes(x=leg_stat,y=mj_pm)) + geom_boxplot()
leg_plot_mj_pm_all <- fca_plot_mj_pm_all + geom_point(alpha=0.2)
leg_plot2_mj_pm_all <- leg_plot_mj_pm_all + stat_summary(fun.y="mean",geom="point", size=4,color="red3")
leg_plot2_mj_pm_all + facet_grid(~yr)

# Visualize change in alcohol use

fca_plot_alc_pm_all <- ggplot(stat_all_08_16,aes(x=leg_stat,y=alc_pm)) + geom_boxplot()
leg_plot_alc_pm_all <- fca_plot_alc_pm_all + geom_point(alpha=0.2)
leg_plot2_alc_pm_all <- leg_plot_alc_pm_all + stat_summary(fun.y="mean",geom="point", size=4,color="red3")


# Test for significant change between 2008 and 2016 alcohol use in decriminalized states

ddimpff_alctest_decrim <- subset(ddimpff, leg_stat=="decrim")
t.test(alc_pm ~ yr, data=ddimpff_alctest_decrim, var.equal=TRUE)

# Test for significant change between 2008 and 2016 alcohol use in states with medicinal marijuana

ddimpff_alctest_med <- subset(ddimpff, leg_stat=="med")
t.test(alc_pm ~ yr, data=ddimpff_alctest_med, var.equal=TRUE)

# Test for significant difference between states with different legalities in just 2016.

ddimpff_mj_change_decrim_n_leg_16 <- subset(ddimpff, leg_stat=="decrim" & yr=="2016" | leg_stat=="legal" & yr=="2016")
t.test(mj_pm ~ leg_stat, data=ddimpff_mj_change_decrim_n_leg_16)

ddimpff_mj_change_decrim_n_no_16 <- subset(ddimpff, leg_stat=="decrim" & yr=="2016" | leg_stat=="no" & yr=="2016")
t.test(mj_pm ~ leg_stat, data=ddimpff_mj_change_decrim_n_no_16)

ddimpff_mj_change_med_n_decrim_16 <- subset(ddimpff, leg_stat=="med" & yr=="2016" | leg_stat=="decrim" & yr=="2016")
t.test(mj_pm ~ leg_stat, data=ddimpff_mj_change_med_n_decrim_16)

ddimpff_mj_change_med_n_no_16 <- subset(ddimpff, leg_stat=="med" & yr=="2016" | leg_stat=="no" & yr=="2016")
t.test(mj_pm ~ leg_stat, data=ddimpff_mj_change_med_n_no_16)

```

Data Analysis II

```{r}

# Create necessary subsets

stat_leg_n_decrim_alc_crash_08 <- subset(stat_all_alc_08, leg_stat=="decrim" | leg_stat=="legal")
stat_leg_n_decrim_drg_crash_08 <- subset(stat_all_drg_08, leg_stat=="decrim" | leg_stat=="legal")
stat_leg_n_decrim_alc_crash_16 <- subset(stat_all_alc_16, leg_stat=="decrim" | leg_stat=="legal")
stat_leg_n_decrim_drg_crash_16 <- subset(stat_all_drg_16, leg_stat=="decrim" | leg_stat=="legal")
stat_leg_n_decrim_drg_crash_08_16 <- subset(stat_all_drg_08_16, leg_stat=="decrim" | leg_stat=="legal")
stat_leg_n_decrim_alc_crash_08_16 <- subset(stat_all_alc_08_16, leg_stat=="decrim" | leg_stat=="legal")

stat_no_n_med_alc_crash_08 <- subset(stat_all_alc_08, leg_stat=="no" | leg_stat=="med")
stat_no_n_med_drg_crash_08 <- subset(stat_all_drg_08, leg_stat=="no" | leg_stat=="med")
stat_no_n_med_alc_crash_16 <- subset(stat_all_alc_16, leg_stat=="no" | leg_stat=="med")
stat_no_n_med_drg_crash_16 <- subset(stat_all_drg_16, leg_stat=="no" | leg_stat=="med")
stat_no_n_med_drg_crash_08_16 <- subset(stat_all_drg_08_16, leg_stat=="no" | leg_stat=="med")
stat_no_n_med_alc_crash_08_16 <- subset(stat_all_alc_08_16, leg_stat=="no" | leg_stat=="med")

# Generate chart of rates for both drug and alcohol involvement in crashes for 2008 and 2016

ggplot(NULL) + geom_point(aes(x=stat_all_drg_08$drg_crash,y=stat_all_drg_08$state), color="green3") + geom_point(aes(x=stat_all_drg_08$alc_crash,y=stat_all_drg_08$state) ,color="chocolate3") + geom_point(aes(x=stat_all_drg_16$drg_crash,y=stat_all_drg_16$state), color="green4") + geom_point(aes(x=stat_all_drg_16$alc_crash,y=stat_all_drg_16$state), color="chocolate4") + xlab("Percent of Accidents Involving Substance") + ylab("State") + coord_cartesian(xlim = c(0, 50)) + theme(axis.title.x = element_text(colour = "red4"), axis.title.y = element_text(colour = "red4"))

# Generate summaries for each (simplified) condition

summary(stat_leg_n_decrim_drg_crash_08$drg_crash)
summary(stat_leg_n_decrim_drg_crash_16$drg_crash)
summary(stat_leg_n_decrim_alc_crash_16$alc_crash)
summary(stat_leg_n_decrim_alc_crash_08$alc_crash)
summary(stat_no_n_med_drg_crash_08$drg_crash)
summary(stat_no_n_med_drg_crash_16$drg_crash)
summary(stat_no_n_med_alc_crash_16$alc_crash)
summary(stat_no_n_med_alc_crash_08$alc_crash)

# Generate chart of percent change between 2008 and 2016 in drug and alcohol involved with crashes

ggplot(NULL) + geom_point(aes(x=fca$state, y=fca$alc_change), color="red") + geom_point(aes(x=fca$state, y=fca$drg_change), color="green4") + coord_cartesian(ylim = c(-15, 15)) + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + ylab("Percent Change, '08-'16") + xlab("State") + theme(axis.title.x = element_text(colour = "red4"), axis.title.y = element_text(colour = "red4"))

# Generate summaries for each condition (and generate necessary subsets)

fca_no <- subset(fca, abrev=="AL" | abrev=="AZ" | abrev=="FL" | abrev=="ID" | abrev=="IL" | abrev=="IN" | abrev=="KS" | abrev=="KY" | abrev=="NJ" | abrev=="NM" | abrev=="PA" | abrev=="SD" | abrev=="TN" | abrev=="UT" | abrev=="VT" | abrev=="WV" | abrev=="WY")
fca_leg <- subset(fca, abrev=="VT" | abrev=="RI" | abrev=="OR" | abrev=="MI" | abrev=="MD" | abrev=="AK")
fca_pot <- subset(fca, abrev=="CO" | abrev=="MN" | abrev=="NE" | abrev=="NV" | abrev=="NC" | abrev=="OH")

summary(fca_no$alc_change)
summary(fca_leg$alc_change)
summary(fca_pot$alc_change)

summary(fca_no$drg_change)
summary(fca_leg$drg_change)
summary(fca_pot$drg_change)

```

Inferential Statistics I

```{r}

# Generate appropriate subsets for each linear model (legal_status_selecting-for-usable-rates>0_year)

stat_all_alc_08 <- subset(ddimpff, alc_crash > 0.0 & yr=="2008")
stat_all_alc_16 <- subset(ddimpff, alc_crash > 0.0 & yr=="2016")
stat_all_alc_08_16 <- subset(ddimpff, alc_crash > 0.0 & yr=="2008" | alc_crash > 0.0 & yr=="2016")
stat_all_drg_08 <- subset(ddimpff, drg_crash > 0.0 & yr=="2008")
stat_all_drg_16 <- subset(ddimpff, drg_crash > 0.0 & yr=="2016")
stat_all_drg_08_16 <- subset(ddimpff, drg_crash > 0.0 & yr=="2008" | drg_crash > 0.0 & yr=="2016")

# Generate linear models (lm_percentage-accidents-involving-substance_as_rate-of-consumption_year)

lm_alc_crash_as_alc_pm_08 <- lm(stat_all_alc_08$alc_crash ~ stat_all_alc_08$alc_pm)
lm_alc_crash_as_alc_pm_16 <- lm(stat_all_alc_16$alc_crash ~ stat_all_alc_16$alc_pm)
lm_alc_crash_as_alc_pm_08_16 <- lm(stat_all_alc_08_16$alc_crash ~ stat_all_alc_08_16$alc_pm)
lm_drg_crash_as_alc_pm_08 <- lm(stat_all_drg_08$drg_crash ~ stat_all_drg_08$alc_pm)
lm_drg_crash_as_alc_pm_16 <- lm(stat_all_drg_16$drg_crash ~ stat_all_drg_16$alc_pm)
lm_drg_crash_as_alc_pm_08_16 <- lm(stat_all_drg_08_16$drg_crash ~ stat_all_drg_08_16$alc_pm)

lm_alc_crash_as_mj_pm_08 <- lm(stat_all_alc_08$alc_crash ~ stat_all_alc_08$mj_pm)
lm_alc_crash_as_mj_pm_16 <- lm(stat_all_alc_16$alc_crash ~ stat_all_alc_16$mj_pm)
lm_alc_crash_as_mj_pm_08_16 <- lm(stat_all_alc_08_16$alc_crash ~ stat_all_alc_08_16$mj_pm)
lm_drg_crash_as_mj_pm_08 <- lm(stat_all_drg_08$drg_crash ~ stat_all_drg_08$mj_pm)
lm_drg_crash_as_mj_pm_16 <- lm(stat_all_drg_16$drg_crash ~ stat_all_drg_16$mj_pm)
lm_drg_crash_as_mj_pm_08_16 <- lm(stat_all_drg_08_16$drg_crash ~ stat_all_drg_08_16$mj_pm)

# Plot models of crash rates as a function of marijuana use

plot(stat_all_08_16$mj_pm, stat_all_08_16$drg_crash, col = "chartreuse4", ylab = 'Percent of Accidents Involving Substance', xlab = "Marijuana Use in the Past Month", main = "Fatal Accidents and Marijuana Use of 18-25 Year-Olds in the Past Month")
points(stat_all_08_16$mj_pm, stat_all_08_16$alc_crash, col = "coral4")
abline(lm_alc_crash_as_mj_pm_16, col='darkorange3')
abline(lm_alc_crash_as_mj_pm_08, col='darkorange2')
abline(lm_alc_crash_as_mj_pm_08_16, col='darkorange4')
abline(lm_drg_crash_as_mj_pm_16, col='darkolivegreen4')
abline(lm_drg_crash_as_mj_pm_08, col='darkolivegreen3')
abline(lm_drg_crash_as_mj_pm_08_16, col='darkolivegreen')

# Plot models of crash rates as a function of alcohol use

plot(stat_all_08_16$alc_pm, stat_all_08_16$drg_crash, col = "chartreuse4", ylab = 'Percent of Accidents Involving Substance', xlab = "Alcohol Use in the Past Month", main = "Fatal Accidents and Marijuana Use of 18-25 Year-Olds in the Past Month")
points(stat_all_08_16$alc_pm, stat_all_08_16$alc_crash, col = "coral4")
abline(lm_drg_crash_as_alc_pm_08_16, col='darkolivegreen')
abline(lm_drg_crash_as_alc_pm_16, col='darkolivegreen4')
abline(lm_drg_crash_as_alc_pm_08, col='darkolivegreen3')
abline(lm_alc_crash_as_alc_pm_08_16, col='darkorange4')
abline(lm_alc_crash_as_alc_pm_16, col='darkorange3')
abline(lm_alc_crash_as_alc_pm_08, col='darkorange2')

# Summarize just the signficant relationships (the third model is just "nearly" significant)

summary(lm_alc_crash_as_mj_pm_16)
summary(lm_drg_crash_as_mj_pm_16)
summary(lm_alc_crash_as_mj_pm_08_16)

summary(lm_alc_crash_as_alc_pm_16)
summary(lm_alc_crash_as_alc_pm_08)
summary(lm_alc_crash_as_alc_pm_08_16)
summary(lm_drg_crash_as_alc_pm_16)

# Generate CI plots for significant relationships

plot_CI(stat_all_alc_16$alc_crash, stat_all_alc_16$mj_pm, 0.95)
plot_CI(stat_all_drg_16$alc_crash, stat_all_drg_16$mj_pm, 0.95)
plot_CI(stat_all_alc_08_16$alc_crash, stat_all_alc_08_16$mj_pm, 0.95)

plot_CI(stat_all_alc_16$alc_crash, stat_all_alc_16$alc_pm, 0.95)
plot_CI(stat_all_alc_08$alc_crash, stat_all_alc_08$alc_pm, 0.95)
plot_CI(stat_all_alc_08_16$alc_crash, stat_all_alc_08_16$alc_pm, 0.95)
plot_CI(stat_all_drg_16$alc_crash, stat_all_drg_16$alc_pm, 0.95)

# Multiple linear regression of substance t.test(drg_crash ~ leg_stat_boolio, data=leg_n_no_slash_med_tdata_new, var.equal=TRUE)

lm(formula = stat_all_drg_08_16$drg_crash ~ stat_all_drg_08_16$alc_pm + 
    stat_all_drg_08_16$mj_pm)

# Multiple linear regression of substance use on alcohol crashes

lm(formula = stat_all_alc_08_16$alc_crash ~ stat_all_alc_08_16$alc_pm + 
    stat_all_alc_08_16$mj_pm)

```

Inferential Statistics II

```{r}

# Generate linear models for drug crashes and marijuana use for each legality group

lm_drg_crash_as_mj_pm_08_16_no_n_med <- lm(stat_no_n_med_drg_crash_08_16$drg_crash ~ stat_no_n_med_drg_crash_08_16$mj_pm)
lm_drg_crash_as_mj_pm_08_no_n_med <- lm(stat_no_n_med_drg_crash_08$drg_crash ~ stat_no_n_med_drg_crash_08$mj_pm)
lm_drg_crash_as_mj_pm_16_no_n_med <- lm(stat_no_n_med_drg_crash_16$drg_crash ~ stat_no_n_med_drg_crash_16$mj_pm)
lm_drg_crash_as_mj_pm_08_16_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_08_16$drg_crash ~ stat_leg_n_decrim_drg_crash_08_16$mj_pm)
lm_drg_crash_as_mj_pm_08_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_08$drg_crash ~ stat_leg_n_decrim_drg_crash_08$mj_pm)
lm_drg_crash_as_mj_pm_16_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_16$drg_crash ~ stat_leg_n_decrim_drg_crash_16$mj_pm)

# Plot relationship of drug crashes and marijuana use for states in which marijuana is legalized/decriminalized and medicinal only/illegal

plot(stat_leg_n_decrim_drg_crash_08$mj_pm, stat_leg_n_decrim_drg_crash_08$drg_crash, col = "chartreuse4", ylab = 'Percent of Accidents Involving Drugs', xlab = "Marijuana Use in the Past Month", main = "legal comparo", ylim=c(0, 45) , xlim=c(5, 45))
points(stat_leg_n_decrim_drg_crash_16$mj_pm, stat_leg_n_decrim_drg_crash_16$drg_crash, col = "green")
points(stat_no_n_med_drg_crash_16$mj_pm, stat_no_n_med_drg_crash_16$drg_crash, col = "red")
points(stat_no_n_med_drg_crash_08$mj_pm, stat_no_n_med_drg_crash_08$drg_crash, col = "pink")

# Label points

with(stat_no_n_med_drg_crash_08[stat_no_n_med_drg_crash_08$abbrev %in% stat_no_n_med_drg_crash_08$abbrev[c(1:100)],],text(x = stat_no_n_med_drg_crash_08$mj_pm, y = stat_no_n_med_drg_crash_08$drg_crash, labels = abbrev, pos = 2, cex=.7, col="pink"))

with(stat_no_n_med_drg_crash_16[stat_no_n_med_drg_crash_16$abbrev %in% stat_no_n_med_drg_crash_16$abbrev[c(1:100)],],text(x = stat_no_n_med_drg_crash_16$mj_pm, y = stat_no_n_med_drg_crash_16$drg_crash, labels = abbrev, pos = 2, cex=.7, col="darkred"))

with(stat_leg_n_decrim_drg_crash_16[stat_leg_n_decrim_drg_crash_16$abbrev %in% stat_leg_n_decrim_drg_crash_16$abbrev[c(1:100)],],text(x = stat_leg_n_decrim_drg_crash_16$mj_pm, y = stat_leg_n_decrim_drg_crash_16$drg_crash, labels = abbrev, pos = 2, cex=.7, col="green"))

with(stat_leg_n_decrim_drg_crash_08[stat_leg_n_decrim_drg_crash_08$abbrev %in% stat_leg_n_decrim_drg_crash_08$abbrev[c(1:100)],],text(x = stat_leg_n_decrim_drg_crash_08$mj_pm, y = stat_leg_n_decrim_drg_crash_08$drg_crash, labels = abbrev, pos = 2, cex=.7, col="darkgreen"))

# Plot linear models

abline(lm_drg_crash_as_mj_pm_08_no_n_med, col='pink')
abline(lm_drg_crash_as_mj_pm_16_no_n_med, col='darkred')
abline(lm_drg_crash_as_mj_pm_16_leg_n_decrim, col='green')
abline(lm_drg_crash_as_mj_pm_08_leg_n_decrim, col='darkgreen')

# Summarize signficant models

summary(lm_drg_crash_as_mj_pm_16_leg_n_decrim)

# Generate linear models for drug crashes and alcohol use for each legality group

lm_drg_crash_as_alc_pm_08_16_no_n_med <- lm(stat_no_n_med_drg_crash_08_16$drg_crash ~ stat_no_n_med_drg_crash_08_16$alc_pm)
lm_drg_crash_as_alc_pm_08_no_n_med <- lm(stat_no_n_med_drg_crash_08$drg_crash ~ stat_no_n_med_drg_crash_08$alc_pm)
lm_drg_crash_as_alc_pm_16_no_n_med <- lm(stat_no_n_med_drg_crash_16$drg_crash ~ stat_no_n_med_drg_crash_16$alc_pm)
lm_drg_crash_as_alc_pm_08_16_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_08_16$drg_crash ~ stat_leg_n_decrim_drg_crash_08_16$alc_pm)
lm_drg_crash_as_alc_pm_08_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_08$drg_crash ~ stat_leg_n_decrim_drg_crash_08$alc_pm)
lm_drg_crash_as_alc_pm_16_leg_n_decrim <- lm(stat_leg_n_decrim_drg_crash_16$drg_crash ~ stat_leg_n_decrim_drg_crash_16$alc_pm)

# Plot relationship of alcohol crashes and use in states in which marijuana is legalized/decriminalized and medicinal only/illegal

plot(stat_leg_n_decrim_drg_crash_08$alc_pm, stat_leg_n_decrim_drg_crash_08$drg_crash, col = "chartreuse4", ylab = 'Percent of Accidents Involving Drugs', xlab = "Alcohol Use in the Past Month", main = "legal comparo", ylim=c(0, 50) , xlim=c(30, 75))
points(stat_leg_n_decrim_drg_crash_16$alc_pm, stat_leg_n_decrim_drg_crash_16$drg_crash, col = "green")
points(stat_no_n_med_drg_crash_16$alc_pm, stat_no_n_med_drg_crash_16$drg_crash, col = "red")
points(stat_no_n_med_drg_crash_08$alc_pm, stat_no_n_med_drg_crash_08$drg_crash, col = "pink")

# Plot linear models
abline(lm_drg_crash_as_alc_pm_08_no_n_med, col='pink')
abline(lm_drg_crash_as_alc_pm_16_no_n_med, col='darkred')
abline(lm_drg_crash_as_alc_pm_16_leg_n_decrim, col='green')
abline(lm_drg_crash_as_alc_pm_08_leg_n_decrim, col='darkgreen')

# Label points

with(stat_no_n_med_drg_crash_08[stat_no_n_med_drg_crash_08$abbrev %in% stat_no_n_med_drg_crash_08$abbrev[c(1:100)],],text(x = stat_no_n_med_drg_crash_08$alc_pm, y = stat_no_n_med_drg_crash_08$drg_crash, labels = abbrev, pos = 2, cex=.7, col="pink"))

with(stat_no_n_med_drg_crash_16[stat_no_n_med_drg_crash_16$abbrev %in% stat_no_n_med_drg_crash_16$abbrev[c(1:100)],],text(x = stat_no_n_med_drg_crash_16$alc_pm, y = stat_no_n_med_drg_crash_16$drg_crash, labels = abbrev, pos = 2, cex=.7, col="darkred"))

with(stat_leg_n_decrim_drg_crash_16[stat_leg_n_decrim_drg_crash_16$abbrev %in% stat_leg_n_decrim_drg_crash_16$abbrev[c(1:100)],],text(x = stat_leg_n_decrim_drg_crash_16$alc_pm, y = stat_leg_n_decrim_drg_crash_16$drg_crash, labels = abbrev, pos = 2, cex=.7, col="green"))

with(stat_leg_n_decrim_drg_crash_08[stat_leg_n_decrim_drg_crash_08$abbrev %in% stat_leg_n_decrim_drg_crash_08$abbrev[c(1:100)],],text(x = stat_leg_n_decrim_drg_crash_08$alc_pm, y = stat_leg_n_decrim_drg_crash_08$drg_crash, labels = abbrev, pos = 2, cex=.7, col="darkgreen"))

# Summarize significant model

summary(lm_drg_crash_as_alc_pm_16_leg_n_decrim)

```

Section 5;

```{r}

# Subset data to exclude null report rates (pre-screened as needing be greater than 20 percent reported)

ddimpfff_clean <- subset(ddimpfff, drg_reprate > 0.0 & drg_crash > 0.0 & alc_reprate > 0.0 & alc_crash > 0.0)

# Check whether or not there is a meaningful relationship between report rates and reported crashes

lm_drg_crash_as_drg_reprate <- lm(ddimpfff_clean$drg_crash ~ ddimpfff_clean$drg_reprate)
summary(lm_drg_crash_as_drg_reprate)
qqplot(ddimpfff_clean$drg_crash, ddimpfff_clean$drg_reprate)

lm_alc_crash_as_alc_reprate <- lm(ddimpfff_clean$alc_crash ~ ddimpfff_clean$alc_reprate)
summary(lm_alc_crash_as_alc_reprate)
qqplot(ddimpfff_clean$alc_crash, ddimpfff_clean$alc_reprate)

```


