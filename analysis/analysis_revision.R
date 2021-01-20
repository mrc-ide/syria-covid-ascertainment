devtools::install_github("mrc-ide/odin","531a28")
devtools::install_github("mrc-ide/squire","v0.4.34")
library(tidyverse)
library(squire)

fig_save <- function(name,
                     fig,
                     width = 6,
                     height = 6,
                     root = file.path(here::here(), "analysis/02_09")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  svg(filename = fig_path(paste0(name,".svg")), width = width, height = height)
  print(fig)
  dev.off()

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()


}

## -----------------------------------------------------------------------------
## REVISION ANALYSIS
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## 1. ASSESSMENT OF INITIAL PROJECTIONS AND SITUATION
## -----------------------------------------------------------------------------

# Get new death data
sheet <- readxl::read_xlsx(file.path(here::here(), "analysis/data/raw/supp_table_4_revision.xlsx"))
daily_df <- sheet %>% pivot_longer(-c((1:3),ncol(sheet),ncol(sheet)-1), names_to = "region", values_to = "deaths") %>%
  select(Date, region, deaths) %>%
  mutate(date = as.Date(Date),
         governorate = region,
         region = factor(c("Damascus", "Other")[as.integer(governorate != "Damascus")+1], levels = rev(c("Damascus", "Other"))))
daily_df$deaths[is.na(daily_df$deaths)] <- 0
daily_df$region <- as.character(daily_df$region)
daily_df$region[daily_df$governorate=="Homs"] <- "Homs"
daily_df$region <- factor(daily_df$region, levels = rev(c("Damascus", "Homs", "Other")))
daily_df <- daily_df[!is.na(daily_df$Date),]

# first figure showing shift in location
fig_revision_0 <- ggplot(daily_df, aes(as.Date(Date), deaths, fill = region)) +
  geom_vline(xintercept = as.Date("2020-09-02"), linetype = "dashed") +
  annotate("label", x = as.Date("2020-09-02")-20, y = 16.5, label = "Date of Initial \nAnalysis", label.padding = unit(0.3, "lines")) +
  geom_vline(xintercept = as.Date("2020-12-01"), linetype = "dashed") +
  annotate("label", x = as.Date("2020-12-01")-20, y = 16.5, label = "End of Intial \nProjection", label.padding = unit(0.3, "lines")) +
  geom_bar(stat="identity", fill = "black", color = NA, width = 1) +
  geom_bar(aes(fill=region), stat = "identity", width = 1) +
  ylab("Daily Reported Deaths\n") +
  theme_bw() +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2020-02-01"), as.Date("2021-01-12")), expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0.01)) +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_viridis_d(option = "E", direction = 1, end = 0.9, name = "Governorate:")


# now comparison of fit to
dam_df <- daily_df[daily_df$governorate == "Damascus",]
dam_df <- complete(dam_df[,-1], date = seq.Date(min(as.Date(dam_df$Date)), max(as.Date(dam_df$Date)), by=1)) %>%
  replace_na(list(region = "Damascus", deaths = 0, governorate = "Damascus"))

fig_revision_1 <- format_output(maintain_4months, "deaths", date_0 = "2020-09-02") %>%
  group_by(date) %>%
  summarise(deaths_low = mean(y, na.rm = TRUE)*0.01,
            deaths = mean(y, na.rm = TRUE)*0.015,
            deaths_high = mean(y, na.rm = TRUE)*0.03) %>%
  ggplot(aes(date, deaths)) +
  geom_vline(xintercept = as.Date("2020-12-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-09-02"), linetype = "dashed") +
  geom_line(color = "#440154", lwd = 2) +
  geom_ribbon(aes(date, ymin = deaths_low, ymax = deaths_high), fill = "#440154", alpha = 0.2) +
  geom_line(aes(date, deaths_high), color = "#440154", linetype = "dashed", lwd = 1) +
  geom_line(aes(date, deaths_low), color = "#440154", linetype = "dashed", lwd = 1) +
  geom_line(aes(date, zoo::rollmean(deaths, 7, na.pad = TRUE)), dam_df, color = "#35B779", lwd = 1) +
  geom_point(aes(date, deaths), dam_df) +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(as.Date("2020-02-01"), as.Date("2021-01-12")), expand = c(0.01,0.01)) +
  theme(axis.line = element_line(), axis.title.x = element_blank()) +
  ylab("Daily Deaths in Damascus\n")
fig_save("revision_fig_1", cowplot::plot_grid(fig_revision_0, fig_revision_1, ncol =1),  width = 12, height = 6)

## -----------------------------------------------------------------------------
## 2. CERTIFICATE UPDATE
## -----------------------------------------------------------------------------

## Damascus death sources
## https://www.facebook.com/damascusgovrnorat/posts/191862302286989
reported <- data.frame("date" = seq.Date(as.Date("2020-07-25"), as.Date("2020-08-01"),1),
                       "deaths" = c(78, 88, 87, 108, 133, 127, 104, 107))

# https://aliqtisadi.com/790464-%D9%8A%D9%88%D8%AC%D8%AF-%D9%81%D9%8A-%D8%AF%D9%85%D8%B4%D9%82-%D8%B3%D8%AA-%D9%85%D9%82%D8%A7%D8%A8%D8%B1/
reported$deaths_low <- reported$deaths - 50
reported$deaths_high <- reported$deaths - 15

# https://www.facebook.com/MEENALMASOOL/photos/a.1277434595713288/3039607002829363/?type=3 - 25 deaths/day
# CBS Statistical Abstract 2019 | 11725 deaths in 2018. 32 deaths/day
reported$deaths <- reported$deaths -32

## Let's also compare to an assumption that there is even more excess mortlaity than historic due to secondary pressure on health systems
reported$extra_deaths <- reported$deaths - 32
reported$extra_deaths_low <- reported$deaths_low - 32
reported$extra_deaths_high <- reported$deaths_high - 32

# Recreate the certificate data source for fitting
df_t <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_df_all.rds"))

# format dates
df_t <- df_t %>% filter(certificate == TRUE, date > "2015-12-31")
dam_certs <- df_t %>% filter(date >= "2017-01-01")

# collate data
df_certs <- dam_certs$date %>% table()
df_certs <- data.frame("date" = names(df_certs), "deaths" = as.numeric(df_certs))
df_certs$month <- lubridate::month(df_certs$date)
df_certs$year <- lubridate::year(df_certs$date)

# Overview of raw data
fig_revision_a <- ggplot(df_t %>% filter(date > "2020-01-01"), aes(date)) + geom_bar(stat = "count") + theme_bw() +
  ylab("Death notifications \nuploaded per day") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0.01,0.01)) +
  geom_hline(yintercept = 120, lwd = 0) +
  scale_y_continuous(expand = c(0,0), labels = c(0,30,60,90,120)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1))

# estimate baseline death negative binomial distributions
baseline <- df_certs %>% filter(date > "2016-12-31" & date < "2020-01-01") %>%
  group_by(month) %>%
  summarise(nb = list(fitdistrplus::fitdist(deaths, "nbinom")))

# draw from these to create an excess mortality trend
# create a realisation
create_realisation <- function(rep = 1, df_certs, baseline) {

  df_certs$baseline <- vapply(df_certs$month, function(x) {
    rnbinom(1, size = baseline$nb[[x]]$estimate[1], mu = baseline$nb[[x]]$estimate[2])
  }, numeric(1))

  # 0.9 reflects that 10% are estimated to be from outside Damascus - mostly rural Damascus
  df_certs$deaths <-  round((df_certs$deaths - df_certs$baseline)*0.9)
  df_certs$cumu_deaths <- cumsum(df_certs$deaths)
  df_certs$rep <- rep
  return(df_certs)

}

# 100 draws and create plot
draws <- lapply(1:100, create_realisation, df_certs = df_certs %>% filter(date > "2019-12-31"), baseline = baseline)
draws <- do.call(rbind, draws)
fig_revision_b <- draws %>% group_by(date) %>%
  summarise(deaths = mean(deaths)) %>%
  mutate(weekly = zoo::rollmean(deaths, 7, na.pad = TRUE)) %>%
  ggplot(aes(as.Date(date), deaths)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 100, lwd = 0) +
  geom_errorbar(aes(as.Date(date), y, ymin = ymin, ymax = ymax, group = date),
                draws %>% group_by(date) %>%
                  summarise(y = mean(deaths), ymin = quantile(deaths, 0.025), ymax = quantile(deaths, 0.975)),
                color = viridis::cividis(1,begin = 0.4),
                alpha = 0.4) +
  geom_line(aes(y = weekly), color = viridis::cividis(1), lwd = 1.5)  +
  theme_bw() +
  ylab("Excess Deaths estimated \nfrom notifications") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(NA,100)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.y = element_line(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1)) +
  geom_point(aes(date, deaths), reported)


# Create certificates to fit to
certs_to_fit <- df_certs %>% filter(date > "2020-06-30" & date < "2021-01-14")
certs_to_fit <- certs_to_fit %>% group_by(month) %>%
  mutate(deaths = round((deaths - baseline$nb[[1]]$estimate[2])*0.9))
certs_to_fit$deaths[certs_to_fit$deaths < 0] <- 0
certs_to_fit$date <- as.Date(certs_to_fit$date)

# save the filtered certificate death data
saveRDS(certs_to_fit, file.path(here::here(), "src/syria_under_reporting/certs_deaths_january.rds"))

# copying to our server location for running
# file.copy(file.path(here::here(), "src/syria_under_reporting/certs_deaths_january.rds"),
#           "/home/oj/net/lmic_new/datadrive/lmic/testing/src/syria_under_reporting/", overwrite = TRUE)

# run on server location
# ./docker/run_syria_underreporting 2021-01-13 2394000 FALSE erts_deaths_january.rds

# # Grab results
# f <- readRDS("/home/oj/net/lmic_new/datadrive/lmic/testing/archive/syria_under_reporting/20210117-122528-ffd77330/res.rds")
# saveRDS(f, file.path(here::here(), "analysis/revision_cert_run.rds"))

## -----------------------------------------------------------------------------
## Post certificate analysis
## -----------------------------------------------------------------------------

# ANnoyingly odin changed so much during revision we had to install the old one
# for analysis pre Christmas and this one for recent analysis :(
devtools::install_github("mrc-ide/odin")
devtools::install_github("mrc-ide/squire")
library(tidyverse)
library(squire)

new <- readRDS(file.path(here::here(), "analysis/revision_cert_run.rds"))
deaths <- format_output(new, "deaths", date_0 = "2021-01-12")
deaths <- deaths %>% group_by(date) %>%
  summarise(deaths = median(y, na.rm = TRUE),
            deaths_low = quantile(y, 0.025, na.rm = TRUE),
            deaths_high = quantile(y, 0.975, na.rm = TRUE))

# reporting fractions overall
sum(dam_df$deaths)/colSums(deaths[deaths$date < as.Date("2021-01-13"),2:4])

# reporting fraction after 2 September
sum(dam_df$deaths[dam_df$date > "2020-09-02"]) / sum(deaths$deaths[deaths$date > "2020-09-02" & deaths$date <= "2021-01-13"], na.rm=TRUE)

# overall new fit
duplicates <- function(x, y) {
  tbl <- table(c(x, y))
  names(tbl[which(tbl>1)])
}

fig_revision_c <- ggplot(deaths[deaths$date<"2021-04-13",], aes(date, deaths)) +
  geom_line(color = "#440154") +
  geom_ribbon(aes(x = date, ymin = deaths_low, ymax = deaths_high), alpha = 0.2, fill = "#440154") +
  geom_point(aes(date, deaths), reported) +
  geom_bar(aes(date, deaths), certs_to_fit, stat = "identity", fill = "#1F9E89", col = "#1F9E89") +
  geom_bar(aes(date, deaths), dam_df, stat = "identity", fill = "#3E4A89", col = "#3E4A89") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0.0,0)) +
  ylab("\nDeaths")


# reporting fraction over time
rf_df <- data.frame(
  "date" = as.Date(duplicates(dam_df$date, unique(deaths$date))),
  "reported" = dam_df$deaths[as.character(dam_df$date) %in% duplicates(dam_df$date, unique(deaths$date))],
  "estimated" = deaths$deaths[as.character(deaths$date) %in% duplicates(dam_df$date, unique(deaths$date))],
  "estimated_low" = deaths$deaths_low[as.character(deaths$date) %in% duplicates(dam_df$date, unique(deaths$date))],
  "estimated_high" = deaths$deaths_high[as.character(deaths$date) %in% duplicates(dam_df$date, unique(deaths$date))]
  ) %>%
  mutate(rf = reported/estimated,
         rf_low = reported/estimated_low,
         rf_high = reported/estimated_high)

fig_revision_d <- ggplot(rf_df[rf_df$date > "2020-07-01",], aes(x = date, y = rf)) +
  geom_line(aes(date, zoo::rollmean(rf, 14, na.pad = TRUE))) +
  geom_ribbon(aes(date, ymin = zoo::rollmean(rf_low, 14, na.pad = TRUE),
                  ymax = zoo::rollmean(rf_high, 14, na.pad = TRUE)),
              alpha = 0.2, fill = "#1F9E89") +
  geom_line(aes(date, zoo::rollmean(rf_low, 14, na.pad = TRUE)), linetype = "dashed") +
  geom_line(aes(date, zoo::rollmean(rf_high, 14, na.pad = TRUE)), linetype = "dashed") +
  ylab("2-week Average Mortality \nReporting Fraction") +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_y_continuous(expand = c(0.01,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0.01,0.01)) +
  theme(axis.line = element_line(), axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2))


revision_fig_2 <- cowplot::plot_grid(fig_revision_a, fig_revision_b,
                   NA, NA,
                   fig_revision_c, fig_revision_d,
                   ncol = 3, byrow = FALSE,
                   rel_widths = c(1, 0.05, 1))
fig_save("revision_fig_2", revision_fig_2,  width = 12, height = 8)
