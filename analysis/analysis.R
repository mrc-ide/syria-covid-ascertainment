library(tidyverse)
library(squire)
library(orderly)
library(here)
library(scales)


version_min <- "0.4.34"
if(packageVersion("squire") != version_min) {
  stop("squire needs to be updated to at least ", version_min)
}

## --------------------------------------------
## SUBMITTING MODEL FITTING
## --------------------------------------------

# ------------------------------------------------------------------------------
# START CODE TO RUN FULL SENSITIVITY ANALYSIS LOCALLY/YOURSELF

# # submission args
# args <- expand.grid(date="2020-09-02",
#                     dam_pop=c(2394000, 4800000),
#                     pho=c(TRUE, FALSE),
#                     ca=c("older","same"),
#                     hosp_beds=c(3300,3800,4300,4800,5300),
#                     normal_hospital_use=c(0.45, 0.55, 0.65),
#                     late_start = FALSE)
#
# # evaluate on cmd line or server etc
# paste0(paste("./docker/run_syria_underreporting", apply(args,1,paste,collapse = " "))[1:60], collapse = "; ")
# paste0(paste("./docker/run_syria_underreporting", apply(args,1,paste,collapse = " "))[61:120], collapse = "; ")
#
# ls_args <- expand.grid(date="2020-09-02",
#                        dam_pop=c(2394000, 4800000),
#                        pho=c(TRUE, FALSE),
#                        ca=c("older","same"),
#                        hosp_beds=c(3300,3800,4300,4800,5300),
#                        normal_hospital_use=c(0.45, 0.55, 0.65),
#                        late_start = TRUE)
#
# # evaluate on cmd line or server etc
# paste0(paste("./docker/run_syria_underreporting", apply(ls_args,1,paste,collapse = " "))[1:60], collapse = "; ")
# paste0(paste("./docker/run_syria_underreporting", apply(ls_args,1,paste,collapse = " "))[61:120], collapse = "; ")
#
# ## --------------------------------------------
# ## FETCHING MODEL FITTING
# ## --------------------------------------------
#
# # Grab results from wherever your server location is and for the date you ran the analysis
# f <- list.files("/home/oj/net/lmic_new/datadrive/lmic/testing/archive/syria_under_reporting", full.names = TRUE)
# f <- grep("20200903|20200904", f, value = TRUE)
# res <- paste0(f, "/likelihood.rds")
#
# # Sort
# ll_list <- pbapply::pblapply(res, readRDS)
#
# # remove tjose that are late_start runs
# ls_keep <- which(unlist(lapply(ll_list, function(x) {!x$late_start})))
#
# ll <- data.table::rbindlist(ll_list[ls_keep], fill = TRUE)
# ll$file <- paste0(f[ls_keep], "/res.rds")
# ll$id <- paste(ll$reporting_fraction, ll$dam_pop, ll$poorer_health_outcomes, ll$city_age, ll$hospital_normal_use, ll$hosp_beds)
# ll$run_time <- unlist(lapply(strsplit(ll$file, "/"), function(x) {
#   lubridate::as_datetime(substr(tail(x, 2)[1], 1, 15),format = "%Y%m%d-%H%M%S")
# }))
#
# # remove duff runs
# ll <- ll[!is.na(ll$hosp_beds),]
# ll <- ll[ll$hosp_beds != 24501,]
# ll <- ll[ll$reporting_fraction != 1,]
#
# # get max runs
# ll <- group_by(ll, id) %>%  slice(which.max(run_time))
# ll <- ll[order(ll$ll_reported, decreasing = TRUE),]
#
# # get beds
# ll$beds <- ll$hosp_beds*(1-ll$hospital_normal_use)
#
# dir.create(file.path(here::here(), "analysis/data/derived/02_09"))
# ll_path <- file.path(here::here(), "analysis/data/derived/02_09/ll.rds")
# saveRDS(ll, ll_path)
# ll <- readRDS(ll_path)
#
# ## --------------------------------------------
# ## FETCHING MODEL WITH LATE START
# ## --------------------------------------------
#
# # Grab results from wherever your server location is and for the date you ran the analysis
# f <- list.files("/home/oj/net/lmic_new/datadrive/lmic/testing/archive/syria_under_reporting", full.names = TRUE)
# f <- grep("2020090", f, value = TRUE)
# res <- paste0(f, "/likelihood.rds")
#
# # Sort
# ll_list <- pbapply::pblapply(res, readRDS)
#
# # remove those that are not late_start runs
# ls_keep <- which(unlist(lapply(ll_list, function(x) {"late_start" %in% names(x) && x$late_start})))
#
# ll <- data.table::rbindlist(ll_list[ls_keep], fill = TRUE)
# ll$file <- paste0(f[ls_keep], "/res.rds")
# ll$id <- paste(ll$reporting_fraction, ll$dam_pop, ll$poorer_health_outcomes, ll$city_age, ll$hospital_normal_use, ll$hosp_beds)
# ll$run_time <- unlist(lapply(strsplit(ll$file, "/"), function(x) {
#   lubridate::as_datetime(substr(tail(x, 2)[1], 1, 15),format = "%Y%m%d-%H%M%S")
# }))
#
# # remove duff runs
# ll <- ll[!is.na(ll$hosp_beds),]
# ll <- ll[ll$hosp_beds != 24501,]
# ll <- ll[ll$reporting_fraction != 1,]
#
# # get max runs
# ll <- group_by(ll, id) %>%  slice(which.max(run_time))
# ll <- ll[order(ll$ll_reported, decreasing = TRUE),]
#
# # get beds
# ll$beds <- ll$hosp_beds*(1-ll$hospital_normal_use)
#
# dir.create("analysis/syria")
# ll_ls_path <- file.path(here::here(), "analysis/data/derived/02_09/ll_ls.rds")
# saveRDS(ll, ll_ls_path)

# FINISH CODE TO RUN SENSITIVITY ANALYSIS LOCALLY/YOURSELF
# ------------------------------------------------------------------------------

# grab the saved outpurs
ll_path <- file.path(here::here(), "analysis/data/derived/02_09/ll.rds")
ll_ls_path <- file.path(here::here(), "analysis/data/derived/02_09/ll_ls.rds")
ll_ls <- readRDS(ll_ls_path)
ll <- readRDS(ll_path)


## --------------------------------------------
## PLOTTING
## --------------------------------------------

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

# date analysis is run on
date_0 <- "2020-09-02"

## --------------------------------------------
# Figure 1
## --------------------------------------------

# Maintaining daily sheet for deaths
sheet <- readxl::read_xlsx(file.path(here::here(), "analysis/data/raw/supp_table_4.xlsx"))

# Formatting sheet for plotting
daily_df <- sheet %>% pivot_longer(Damascus:Latakia, names_to = "region", values_to = "deaths") %>%
  select(Date, region, deaths) %>%
  mutate(date = as.Date(Date),
         governorate = region,
         region = factor(c("Damascus", "Other")[as.integer(governorate != "Damascus")+1], levels = rev(c("Damascus", "Other"))))
daily_df$deaths[is.na(daily_df$deaths)] <- 0
daily_df$Date <- as.Date(daily_df$Date)
daily_df <- filter(daily_df, Date <= date_0)

# Get ACAPs
download_url <- function(url, ext = "") {
  tryCatch({
    tf <- tempfile(fileext = ext)
    code <- download.file(url, tf, mode = "wb")
    if (code != 0) {
      stop("Error downloading file")
    }
  },
  error = function(e) {
    stop(sprintf("Error downloading file '%s': %s, please check %s",
                 url, e$message))
  })
  return(tf)
}
acap_site <- "http://www.acaps.org/covid19-government-measures-dataset"
xml <- xml2::read_html(acap_site)
url <- rvest::html_attr(rvest::html_nodes(xml, ".file a"), "href")
acap_tf <- download_url(url, ".xlsx")
acap <- readxl::read_excel(acap_tf, progress = FALSE, sheet = 2)
acap_missing <- readxl::read_excel(file.path(here::here(), "analysis/data/raw/acaps_missing.xlsx"), progress = FALSE)
acap <- rbind(acap, acap_missing)

# country name fixes. We want to use the ISO3C eventually but there are typos...
acap$ISO <- countrycode::countrycode(acap$COUNTRY, "country.name", "iso3c",
                                     custom_match = c("Eswatini"="SWZ", "Micronesia"="FSM"))
acap_syr <- acap %>% filter(ISO == "SYR") %>% arrange_at("DATE_IMPLEMENTED")
write_tsv(acap_syr, file.path(here::here(), "analysis/data/raw/supp_table_5.csv"))
acap_syr <- read.csv(file.path(here::here(), "analysis/data/raw/supp_table_5.csv"), sep = "\t")

# load in the brt model used
brt <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/brt.rds"))

# Plot
fig1 <- ggplot(daily_df, aes(as.Date(Date), deaths, fill = region)) +
  geom_vline(aes(xintercept = as.Date(DATE_IMPLEMENTED)), data = acap_syr, linetype = "solid", lwd = 0.2, color = "grey") +
  geom_hline(aes(yintercept = 5.15), lwd = 0, color = "grey") +
  geom_point(aes(x = date, y = 5.1), data = brt$SYR[which(diff(brt$SYR$C)>0)+1,], color = "orange", inherit.aes = FALSE) +
  geom_point(aes(x = date, y = 5.1), data = brt$SYR[which(diff(brt$SYR$C)<0)+1,], color = "purple", inherit.aes = FALSE) +
  geom_step(aes(x = date, y = C*4.8), brt$SYR %>% filter(date>as.Date("2020-03-08") & date<as.Date(date_0)),
            inherit.aes = FALSE, lwd = 1.5, color = "#CA3433") +
  geom_bar(stat="identity", fill = "black", color = NA, width = 1) +
  geom_bar(aes(fill=region), stat = "identity", width = 1) +
  ylab("Daily Reported Deaths\n") +
  theme_bw() +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*0.2, name="Inferred Mobility\n", labels = scales::percent_format(accuracy = 1)), expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_viridis_d(option = "E", direction = 1, end = 0.8, name = "Governorate:")

fig_save("fig1", fig1, height = 4, width = 8)


## ------------------------------------------------
# FIGURE 2b Likelihood
## ------------------------------------------------

ll$reporting_fraction_factor <- factor(
  scales::percent(ll$reporting_fraction,accuracy = 0.01),
  levels = rev(c("0.05%", "0.10%", "0.50%", "1.00%", "1.25%", "1.50%", "2.00%", "3.00%", "6.00%", "10.00%", "20.00%"))
)

ll_ls$reporting_fraction_factor <- factor(
  scales::percent(ll_ls$reporting_fraction,accuracy = 0.01),
  levels = rev(c("0.05%", "0.10%", "0.50%", "1.00%", "1.25%", "1.50%", "2.00%", "3.00%", "6.00%", "10.00%", "20.00%"))
)

library(scales)
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf))
}

fig2b <- ll %>% pivot_longer(cols = ll_reported:ll_extra) %>%
  mutate(name = c("32 deaths/day baseline", "64 deaths/day baseline")[match(name, c("ll_reported","ll_extra"))],
         color = reporting_fraction_factor) %>%
  filter(reporting_fraction < 0.2) %>%
  ggplot(aes(x = reporting_fraction_factor, y = -value, color = color)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~name, scales = "free_x") +
  scale_y_continuous(trans=reverselog_trans(base=10),
                     labels=trans_format("identity", function(x) -x)) +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank()) +
  coord_flip() +
  scale_color_manual(name = "Reporting Fraction",
                     values = viridis::plasma(12)) +
  ylab("Log Likelihood") +
  xlab("% COVID-19 Deaths Reported")

# ------------------------------------------------
## FIGURE 2a Visual Representation
# ------------------------------------------------

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

# Get runs for the default parameters
default_params <- ll %>% filter(dam_pop == 2394000,
              city_age == "same",
              hospital_normal_use == 0.55,
              hosp_beds == 4300,
              poorer_health_outcomes == FALSE)
# res <- lapply(default_params$file, readRDS)
# groan - remove the last one as too large for github otherwise...
# saveRDS(res, file.path(here::here(), "analysis/data/derived/02_09/default_param_res.RDS"))

res <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/default_param_res.RDS"))

gg1 <- squire::projection_plotting(res,
                          scenarios = as.character(default_params$reporting_fraction_factor),
                          add_parms_to_scenarios = FALSE,
                          var_select = "deaths",
                          ci = FALSE,
                          summary_f = median,
                          date_0 = date_0,
                          x_var = "date")

# fine tuning aes and ordering
gg1$layers[[1]]$mapping$lwd <- 1
gg1$layers[[1]]$data$Scenario <- factor(gg1$layers[[1]]$data$Scenario,
                                       levels = (levels(default_params$reporting_fraction_factor)))
data <- res[[1]]$pmcmc_results$inputs$data

# make plot
fig2a <- gg1 + geom_bar(aes(x=date, y=deaths), data = data,  color = "black", fill = "black", stat = "identity") +
  geom_segment(aes(x = date, xend = date, y = extra_deaths, yend = deaths), size = 0.5, data = reported) +
  geom_point(aes(x = date, y = deaths), shape = 20, data = reported) +
  geom_point(aes(x = date, y = extra_deaths), shape = 25, data = reported, fill = "black", size = 1) +
  scale_color_manual(name = "% Deaths Reported",
                     values = viridis::plasma(12),
                     guide = guide_legend(reverse = TRUE)) +
  xlab("") +
  ylab("Daily COVID-19 Deaths in Damascus\n") +
  guides(linetype = FALSE) +
  scale_x_date(limits = c(min(data$date)-1,max(data$date)+1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,200)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(color = "black"),
        legend.position = c(0.15,0.60))

# ------------------------------------------------
## FIGURE 2c Capacity
# ------------------------------------------------

gg2 <- squire::projection_plotting(res,
                          scenarios = as.character(default_params$reporting_fraction_factor),
                          add_parms_to_scenarios = FALSE,
                          var_select = "hospital_occupancy",
                          ci = FALSE,
                          summary_f = median,
                          date_0 = date_0,
                          x_var = "date")

# fine tuning aes and ordering
gg2$layers[[1]]$mapping$lwd <- 1
gg2$layers[[1]]$data$Scenario <- factor(gg2$layers[[1]]$data$Scenario,
                                       levels = (levels(default_params$reporting_fraction_factor)))

fig2c <- gg2 +
  geom_hline(yintercept = res[[1]]$parameters$hosp_bed_capacity, linetype = "dashed") +
  annotate("rect", xmin=as.Date("2020-07-17"), xmax=as.Date("2020-07-30"),
           ymin=-Inf, ymax=res[[1]]$parameters$hosp_bed_capacity, alpha=0.2, fill="#B80F0A") +
  scale_color_manual(name = "% Deaths Reported",
                     values = viridis::plasma(12),
                     guide = guide_legend(reverse = TRUE)) +
  scale_x_date(limits = range(data$date), expand = c(0.01,0.01)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        legend.position = "none") +
  xlab("Date") +
  ylab("Hospital Bed Occupancy") +
  guides(linetype = FALSE)

# Combine the Figure 2s
fig2 <- cowplot::plot_grid(
  fig2a,
  cowplot::plot_grid(fig2b, NA, fig2c, ncol=3, rel_widths = c(1.5,0.1,1), labels = c("","c")),
  ncol = 1, rel_heights = c(1.2,1), labels = "auto")
fig_save("fig2", fig2, width = 8, height = 8)

# ------------------------------------------------
## FIGURE 3 Current Picture
# ------------------------------------------------

# get the best fiting simulation from the default parameter run
maintain_4months <- res[[which(unlist(lapply(res, function(x) {x$pmcmc_results$inputs$pars_obs$phi_death})) == default_params$reporting_fraction[1])]]

overview_plot <- function(maintain_4months, date_0, date_end = NULL) {

  if(is.null(date_end)) {
    date_end <- tail(rownames(maintain_4months$output),1)
  }
  date_end <- as.Date(date_end)

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


  data <- maintain_4months$pmcmc_results$input$data

ar_plot <- squire::format_output(maintain_4months, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(maintain_4months$parameters$population) - median(y))/sum(maintain_4months$parameters$population)) %>%
  na.omit %>%
  group_by(t, compartment, date) %>%
  summarise(ymin = stats::quantile(ar, 0.025),
            ymax = stats::quantile(ar, 0.975),
            y = median(ar)) %>%
  ggplot(aes(date, y = y, ymin = ymin, ymax = ymax, fill = compartment, color = compartment)) +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  geom_line() +
  geom_ribbon(alpha = 0.25, col = NA) +
  scale_fill_viridis_d(option = "magma",end = 0.5, direction = -1) +
  scale_color_viridis_d(option = "magma",end = 0.5, direction = -1) +
  theme_bw() + ylab("Attack Rate") +
  theme(axis.title.x = element_blank(), legend.position = "none",
        panel.grid.minor.y = element_blank()) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               limits = c(as.Date(rownames(maintain_4months$output)[1]),date_end))

infections <- plot(maintain_4months, "infections", date_0 = date_0, x_var = "date", summary_f = median) +
  theme_bw() + ylab("Daily Infections") + theme(legend.position = "none") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  scale_fill_viridis_d(option = "cividis") +
  scale_color_viridis_d(option = "cividis") +
  theme(axis.title.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               limits = c(as.Date(rownames(maintain_4months$output)[1]),date_end))

deaths_forecast <- plot(maintain_4months, "deaths", date_0 = date_0, x_var = "date", summary_f = median) +
  theme_bw() + ylab("Daily Deaths") + theme(legend.position = "none") +
  #geom_segment(aes(x = date, xend = date, y = extra_deaths, yend = deaths), size = 0.5, data = reported) +
  geom_point(aes(x = date, y = deaths), shape = 20, data = reported) +
  #geom_point(aes(x = date, y = extra_deaths), shape = 25, data = reported, fill = "black", size = 1) +
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  geom_bar(aes(x = date, y = deaths), data = data, stat = "identity") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  theme(axis.title.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               limits = c(as.Date(rownames(maintain_4months$output)[1]),date_end))

healthcare_forecast <- plot(maintain_4months, "hospital_occupancy", date_0 = date_0, x_var = "date", summary_f = median)+
  theme_bw() + ylab("Hospital Occupacy") + theme(legend.position = "none") +
  scale_fill_viridis_d(option = "plasma") +
  scale_color_viridis_d(option = "plasma") +
  geom_hline(yintercept = maintain_4months$parameters$hosp_bed_capacity, linetype = "dotted") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  theme(axis.title.x = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b",
               limits = c(as.Date(rownames(maintain_4months$output)[1]),date_end))

fig3 <- cowplot::plot_grid(
  cowplot::plot_grid(deaths_forecast, NA, infections, healthcare_forecast, NA, ar_plot, labels = c("a","","b","c","","d"), ncol = 3, rel_widths = c(1,0.01,1)),
  NA, rel_widths = c(1,0.01))
return(fig3)
}

fig3 <- overview_plot(maintain_4months, date_0, "2020-12-02")
fig_save("fig3", fig3, width = 8, height = 6)

## Numbers for manuscript

deaths_to_date <- maintain_4months %>% squire::format_output("D") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

active_infections <- squire::format_output(maintain_4months, c("S","R","D"), date_0 = date_0) %>%
  pivot_wider(names_from = compartment, values_from = y) %>%
  mutate(active = sum(maintain_4months$parameters$population)-D-R-S) %>%
  filter(t == 0) %>% select(active) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

daily_infections <- maintain_4months %>% squire::format_output("infections") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

attack_rate <- format_output(maintain_4months, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(maintain_4months$parameters$population) - median(y))/sum(maintain_4months$parameters$population)) %>%
  na.omit %>% filter(t == 0) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

admissions <- maintain_4months %>% squire::format_output(c("hospital_incidence","ICU_incidence"), date_0 = date_0) %>%
  group_by(t,replicate, date) %>% summarise(y=sum(y,na.rm=TRUE)) %>%
  ungroup %>% group_by(t, date) %>% summarise(ymed=median(y,na.rm=TRUE), ymin=quantile(y, 0.025), ymax=quantile(y, 0.975))

days_in_2020 <- length(seq.Date(as.Date(date_0), as.Date("2020-12-31"),1))
longer <- squire::projections(maintain_4months, R0_change = 1, tt_R0 = 0, time_period = days_in_2020)
attack_rate_2021 <- format_output(longer, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(maintain_4months$parameters$population) - median(y))/sum(maintain_4months$parameters$population)) %>%
  na.omit %>% filter(t == days_in_2020) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))


# ------------------------------------------------
## FiG 4: Late Epidemic start Analysis
# ------------------------------------------------

# get the optimum results for default ll_ls
# default_params_ls <- ll_ls %>% filter(dam_pop == 2394000,
#                                 city_age == "same",
#                                 hospital_normal_use == 0.55,
#                                 hosp_beds == 4300,
#                                 poorer_health_outcomes == FALSE)

# ls_out <- readRDS(default_params_ls$file[which.max(default_params_ls$ll_reported)])
# saveRDS(ls_out, file.path(here::here(), "analysis/data/derived/02_09/late_start_best.rds"))

ls_out <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/late_start_best.rds"))
out <- maintain_4months

plot_ls <- function(out, ls_out) {

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


ds_ls_reported <- format_output(ls_out, "deaths_treatment", date_0 = date_0) %>%
  mutate(y = y * ls_out$pmcmc_results$inputs$pars_obs$phi_death)
ds_ls_reported_quant <- group_by(ds_ls_reported, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

ds_reported <- format_output(out, "deaths", date_0 = date_0) %>%
  mutate(y = y * out$pmcmc_results$inputs$pars_obs$phi_death)
ds_reported_quant <- group_by(ds_reported, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

first_date <- min(c(min(ds_ls_reported$date, na.rm=TRUE), min(ds_reported$date, na.rm=TRUE)), na.rm=TRUE)

cols <- viridis::cividis(2, end = 0.7, begin = 0.3)

g1 <- ggplot(ls_out$pmcmc_results$inputs$data, aes(date, deaths)) +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[1],alpha = 0.2, ds_ls_reported_quant, inherit.aes = FALSE) +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[2],  alpha = 0.2, ds_reported_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[1], lwd = 1, data = ds_ls_reported_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[2], lwd = 1, data = ds_reported_quant, inherit.aes = FALSE) +
  geom_point() +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(first_date, date_0)) +
  xlab("") +
  ylab("Ascertained Deaths\n") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  theme(axis.line = element_line())


ds_ls <- format_output(ls_out, "deaths", date_0 = date_0)
ds_ls_quant <- group_by(ds_ls, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

ds <- format_output(out, "deaths", date_0 = date_0)
ds_quant <- group_by(ds, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

g2 <- ggplot(reported, aes(date, deaths)) +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[1],alpha = 0.2, ds_ls_quant, inherit.aes = FALSE) +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[2],  alpha = 0.2, ds_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[1], lwd = 1, data = ds_ls_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[2], lwd = 1, data = ds_quant, inherit.aes = FALSE) +
  geom_point() +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(first_date, date_0)) +
  xlab("") +
  ylab("Excess Deaths\n") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  theme(axis.line = element_line())


hospital_ls_occ <- format_output(ls_out, "hospital_occupancy", date_0 = date_0)
hospital_ls_occ_quant <- group_by(hospital_ls_occ, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

hospital_occ <- format_output(out, "hospital_occupancy", date_0 = date_0)
hospital_occ_quant <- group_by(hospital_occ, date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
            ymax = quantile(y, 0.975, na.rm = TRUE),
            y = median(y, na.rm = TRUE))

g3 <- ggplot(reported, aes(date, deaths)) +
  geom_hline(yintercept = out$parameters$hosp_bed_capacity, linetype = "dotted") +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[1],alpha = 0.2, hospital_ls_occ_quant, inherit.aes = FALSE) +
  geom_ribbon(aes(date, ymin = ymin, ymax = ymax), fill = cols[2],  alpha = 0.2, hospital_occ_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[1], lwd = 1, data = hospital_ls_occ_quant, inherit.aes = FALSE) +
  geom_line(aes(date, y), color = cols[2], lwd = 1, data = hospital_occ_quant, inherit.aes = FALSE) +
  annotate("rect", xmin=as.Date("2020-07-17"), xmax=as.Date("2020-07-30"),
           ymin=-Inf, ymax=out$parameters$hosp_bed_capacity, alpha=0.2, fill="#B80F0A") +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(first_date, date_0)) +
  xlab("") +
  ylab("Hospital Bed Occupancy\n") +
  geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
  theme(axis.line = element_line())

cowplot::plot_grid(g1,g2,g3, ncol = 3, labels = "auto")

}

fig4 <- plot_ls(out, ls_out)
fig_save("fig4", fig4, width = 12, height = 4)

supp_ls_overview <- overview_plot(ls_out, date_0)
fig_save("supp_ls", supp_ls_overview, width = 8, height = 8)

## Numbers for manuscript

deaths_to_date <- ls_out %>% squire::format_output("D") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

active_infections <- squire::format_output(ls_out, c("S","R","D"), date_0 = date_0) %>%
  pivot_wider(names_from = compartment, values_from = y) %>%
  mutate(active = sum(ls_out$parameters$population)-D-R-S) %>%
  filter(t == 0) %>% select(active) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

daily_infections <- ls_out %>% squire::format_output("infections") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

attack_rate <- format_output(ls_out, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(ls_out$parameters$population) - median(y))/sum(ls_out$parameters$population)) %>%
  na.omit %>% filter(t == 0) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

admissions <- ls_out %>% squire::format_output(c("hospital_incidence","ICU_incidence"), date_0 = date_0) %>%
  group_by(t,replicate, date) %>% summarise(y=sum(y,na.rm=TRUE)) %>%
  ungroup %>% group_by(t, date) %>% summarise(ymed=median(y,na.rm=TRUE), ymin=quantile(y, 0.025), ymax=quantile(y, 0.975))

days_in_2020 <- length(seq.Date(as.Date(date_0), as.Date("2020-12-31"),1))
longer <- squire::projections(ls_out, R0_change = 1, tt_R0 = 0, time_period = days_in_2020)
attack_rate_2021 <- format_output(longer, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(ls_out$parameters$population) - median(y))/sum(ls_out$parameters$population)) %>%
  na.omit %>% filter(t == days_in_2020) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))


## ------------------------------------------------
# FiG 5a/b: Wafiat Data Description
# ------------------------------------------------

## First Rows from Wafit Analysis - this was downloaded on the 8th September

df_t <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_df.rds"))
df_t <- df_t %>% filter(certificate == TRUE, date <= "2020-09-02", date > "2015-12-31")

fig5a <- ggplot(df_t, aes(year_month_dated)) + geom_bar(stat = "count") + theme_bw() +
  ylab("Death notifications \nuploaded per month\n") +
  xlab("") +
  geom_hline(yintercept = 1200, lwd = 0) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
  scale_y_continuous(expand = c(0,0)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.y = element_line(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2),
        axis.text.x = element_text(angle = 45,hjust = 1))


fig5b <- ggplot(df_t %>% filter(date > "2020-01-01"), aes(date)) + geom_bar(stat = "count") + theme_bw() +
  ylab("Death notifications \nuploaded per day\n") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
  geom_hline(yintercept = 120, lwd = 0) +
  scale_y_continuous(expand = c(0,0), labels = c(0,30,60,90,120)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.y = element_line(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1))

## ------------------------------------------------
# FiG 5c: Excess Death demonstration for July
# ------------------------------------------------

dam_certs <- df_t %>% filter(date >= "2017-01-01")
dam_certs$month <- lubridate::month(dam_certs$date)
dam_certs$year <- lubridate::year(dam_certs$date)

# collate data
df_certs <- dam_certs$date %>% table()
df_certs <- data.frame("date" = names(df_certs), "deaths" = as.numeric(df_certs))
df_certs$month <- lubridate::month(df_certs$date)
df_certs$year <- lubridate::year(df_certs$date)

# naive annual deaths and relation to annual deaths
fraction_certs <- df_certs %>% group_by(year) %>%
  summarise(deaths = round(sum(deaths)*0.9))
fraction_certs$fraction <- fraction_certs$deaths / c(12135, 11680, 12748, NA)

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
fig5c <- draws %>% group_by(date) %>%
  summarise(deaths = mean(deaths)) %>%
  mutate(weekly = zoo::rollmean(deaths, 7, na.pad = TRUE)) %>%
  ggplot(aes(as.Date(date), deaths)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(as.Date(date), y, ymin = ymin, ymax = ymax, group = date),
                draws %>% group_by(date) %>%
                  summarise(y = mean(deaths), ymin = quantile(deaths, 0.025), ymax = quantile(deaths, 0.975)),
                color = viridis::cividis(1,begin = 0.4),
                alpha = 0.4) +
  geom_line(aes(y = weekly), color = viridis::cividis(1), lwd = 1.5)  +
  theme_bw() +
  ylab("Excess Deaths estmated \nfrom notifications\n") +
  xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line.y = element_line(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.2),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1)) +
  geom_point(aes(date, deaths), reported)


fig5_left <- cowplot::plot_grid(NA, fig5a, NA, fig5b, NA, fig5c, ncol = 2,
                                labels = c("a","","b","","c",""),
                                rel_widths = c(0.05,1))

certs_to_fit <- df_certs %>% filter(date > "2020-06-30")
certs_to_fit <- certs_to_fit %>% group_by(month) %>%
  mutate(deaths = round((deaths - baseline$nb[[1]]$estimate[2])*0.9))
certs_to_fit$deaths[certs_to_fit$deaths < 0] <- 0
certs_to_fit$date <- as.Date(certs_to_fit$date)

# save the filtered certificate death data
saveRDS(certs_to_fit, file.path(here::here(), "src/syria_under_reporting/certs_deaths_july.rds"))

# copying to our server location for running
# file.copy(file.path(here::here(), "src/syria_under_reporting/certs_deaths_july.rds"),
#           "/home/oj/net/lmic_new/datadrive/lmic/testing/src/syria_under_reporting/", overwrite = TRUE)

## ------------------------------------------------
# FiG 5d: Fitting to certificate data
# ------------------------------------------------

# ------------------------------------------------------------------------------
# START CODE TO RUN FULL CERTIFICATE ANALYSIS LOCALLY/YOURSELF

# args <- expand.grid(date="2020-09-02",
#                     dam_pop=c(2394000),
#                     pho=c(FALSE),
#                     ca=c("same"),
#                     hosp_beds=c(4300),
#                     normal_hospital_use=c(0.55),
#                     late_start = FALSE,
#                     data_to_fit = "certs_deaths_july.rds")
#
# # copy these to our orderly parallel run script (run_syria_under_reporting.sh) where ever it is being run. to our server location
# reporting_fractions <- c(0.275, 0.35, 0.4, 0.55, 0.6, 0.65, 0.7, 1)
#
# # evaluate on cmd line or server etc
# paste0(paste("./docker/run_syria_underreporting", apply(args,1,paste,collapse = " ")), collapse = "; ")
#
# # Grab results
# f <- list.files("/home/oj/net/lmic_new/datadrive/lmic/testing/archive/syria_under_reporting", full.names = TRUE)
# f <- grep("20200910|20200910", f, value = TRUE)
# res <- paste0(f, "/likelihood.rds")
#
# # Sort
# ll_list <- pbapply::pblapply(res, readRDS)
#
# # remove those that are not certs
# ls_keep <- which(unlist(lapply(ll_list, function(x) {x$data_to_fit == "certs_deaths_july.rds"})))
#
# ll <- data.table::rbindlist(ll_list[ls_keep], fill = TRUE)
# ll$file <- paste0(f[ls_keep], "/res.rds")
# ll$id <- paste(ll$reporting_fraction, ll$dam_pop, ll$poorer_health_outcomes, ll$city_age, ll$hospital_normal_use, ll$hosp_beds)
# ll$run_time <- unlist(lapply(strsplit(ll$file, "/"), function(x) {
#   lubridate::as_datetime(substr(tail(x, 2)[1], 1, 15),format = "%Y%m%d-%H%M%S")
# }))
#
# # get max runs
# ll <- group_by(ll, id) %>%  slice(which.max(run_time))
# ll <- ll[order(ll$ll_reported, decreasing = TRUE),]
# ll <- ll[ll$reporting_fraction %in% reporting_fractions,]
#
# # get the  100% reporting run, the best fitting run and the smallest reporting run
# cert_runs <- lapply(c(ll$file[ll$reporting_fraction==1],
#                       ll$file[which(!ll$reporting_fraction %in% c(1, reporting_fractions[1]))[1]],
#                       ll$file[ll$reporting_fraction == reporting_fractions[1]]), readRDS)
#
# saveRDS(cert_runs, file.path(here::here(), "analysis/cert_runs.rds"))

# FINISH CODE TO RUN FULL CERTIFICATE ANALYSIS LOCALLY/YOURSELF
# ------------------------------------------------------------------------------

# load in the outputs
cert_runs <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/cert_runs.rds"))

###
plot_certs <- function(cert_runs) {

  # what are the scenarios
  scens <- paste0(unlist(lapply(cert_runs, function(x){
    x$pmcmc_results$inputs$pars$pars_obs$phi_death
  }))*100, "%")

  # date grab
  date_0 <- max(cert_runs[[1]]$pmcmc_results$inputs$data$date)

  # raw date
  raw_data <- cert_runs[[1]]$pmcmc_results$inputs$data

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

  ## 1. Deaths plot:
  # ----------------------------------------------------------------------------

  deaths <- lapply(cert_runs, format_output, var_select = "deaths", date_0 = date_0)
  for(i in seq_along(deaths)) {
    deaths[[i]]$scenario <- scens[i]
  }

  deaths <- dplyr::bind_rows(deaths) %>%
  group_by(date, scenario) %>%
    summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
              y25 = quantile(y, 0.25, na.rm = TRUE),
              y75 = quantile(y, 0.75, na.rm = TRUE),
              ymax = quantile(y, 0.975, na.rm = TRUE),
              y = median(y, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(scenario = factor(scenario, levels = scens))


  g1 <- ggplot(deaths, aes(date, y, fill = scenario, color = scenario)) +
    geom_bar(aes(date, deaths), raw_data, stat = "identity", inherit.aes = FALSE) +
    geom_ribbon(aes(date, ymin = ymin, ymax = ymax), alpha = 0.4, color = NA) +
    geom_ribbon(aes(date, ymin = y25, ymax = y75), alpha = 0.8,color = NA) +
    geom_hline(yintercept = 0, lwd = 0) +
    geom_point(aes(date, deaths), reported, inherit.aes = FALSE) +
    ggpubr::theme_pubclean(base_size = 14) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b", limits = c(min(deaths$date[deaths$ymax>0], na.rm = TRUE), date_0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab("") +
    ylab("Excess Deaths\n") +
    geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
    viridis::scale_fill_viridis(discrete = TRUE,begin = 0.2, end = 0.8, direction = -1, option = "D",
                                name = "% of Total COVID-19 Deaths Ascertained \nFrom Excess Death Notifications:\n") +
    theme(legend.position = "top") +
    guides(fill = guide_legend(title.position = "top")) +
    theme(axis.line.y = element_line(),
          legend.key.width = unit(60, "pt"),
          legend.key.height = unit(15, "pt"),
          legend.title.align = 0.5,
          panel.grid.major.x = element_line(colour = "grey", size = 0.2),
          axis.text.x = element_text(angle = 45,hjust = 1))
  leg <- cowplot::get_legend(g1)
  g1 <- g1 + theme(legend.position = "none")

  ## 2. Hospital Occupancy plot:
  # ----------------------------------------------------------------------------

  hosp <- lapply(cert_runs, format_output, var_select = "hospital_occupancy", date_0 = date_0)
  for(i in seq_along(hosp)) {
    hosp[[i]]$scenario <- scens[i]
  }

  hosp <- dplyr::bind_rows(hosp) %>%
    group_by(date, scenario) %>%
    summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
              y25 = quantile(y, 0.25, na.rm = TRUE),
              y75 = quantile(y, 0.75, na.rm = TRUE),
              ymax = quantile(y, 0.975, na.rm = TRUE),
              y = median(y, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(scenario = factor(scenario, levels = scens))


  g2 <- ggplot(hosp, aes(date, y, fill = scenario, color = scenario)) +
    annotate("rect", xmin=as.Date("2020-07-17"), xmax=as.Date("2020-07-30"),
             ymin=-Inf, ymax=cert_runs[[1]]$parameters$hosp_bed_capacity, alpha=0.2, fill="#B80F0A") +
    geom_hline(yintercept = cert_runs[[1]]$parameters$hosp_bed_capacity, linetype = "dotted") +
    geom_hline(yintercept = c(0,2000), lwd = 0) +
    geom_ribbon(aes(date, ymin = ymin, ymax = ymax), alpha = 0.4, color = NA) +
    geom_ribbon(aes(date, ymin = y25, ymax = y75), alpha = 0.8,color = NA) +
    ggpubr::theme_pubclean(base_size = 14) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b", limits = c(min(deaths$date[deaths$ymax>0], na.rm = TRUE), date_0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab("") +
    ylab("Hospital Occupancy\n") +
    geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
    viridis::scale_fill_viridis(discrete = TRUE,begin = 0.2, end = 0.8, direction = -1, option = "D") +
    theme(axis.line.y = element_line(), legend.position = "none",
          panel.grid.major.x = element_line(colour = "grey", size = 0.2),
          axis.text.x = element_text(angle = 45,hjust = 1))

  ## 3. Attack Rate plot:
  # ----------------------------------------------------------------------------

  ars <- lapply(cert_runs, format_output, var_select = "S", date_0 = date_0)
  for(i in seq_along(ars)) {
    ars[[i]]$scenario <- scens[i]
    ars[[i]]$y <- 1-((ars[[i]]$y)/sum(cert_runs[[i]]$parameters$population))
  }

  ars <- dplyr::bind_rows(ars) %>%
    group_by(date, scenario) %>%
    summarise(ymin = quantile(y, 0.025, na.rm = TRUE),
              y25 = quantile(y, 0.25, na.rm = TRUE),
              y75 = quantile(y, 0.75, na.rm = TRUE),
              ymax = quantile(y, 0.975, na.rm = TRUE),
              y = median(y, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(scenario = factor(scenario, levels = scens))


  g3 <- ggplot(ars, aes(date, y, fill = scenario, color = scenario)) +
    geom_ribbon(aes(date, ymin = ymin, ymax = ymax), alpha = 0.4, color = NA) +
    geom_ribbon(aes(date, ymin = y25, ymax = y75), alpha = 0.8,color = NA) +
    ggpubr::theme_pubclean(base_size = 14) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b", limits = c(min(deaths$date[deaths$ymax>0], na.rm = TRUE), date_0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab("") +
    ylab("Attack Rate\n") +
    geom_vline(xintercept = as.Date(date_0), linetype = "dashed") +
    geom_hline(yintercept = 0, lwd = 0) +
    viridis::scale_fill_viridis(discrete = TRUE,begin = 0.2, end = 0.8, direction = -1, option = "D") +
    theme(axis.line.y = element_line(), legend.position = "none",
          panel.grid.major.x = element_line(colour = "grey", size = 0.2),
          axis.text.x = element_text(angle = 45,hjust = 1))

  fig5def <- cowplot::plot_grid(NA, g1, NA, g2, NA, g3, ncol = 2,
                     labels = c("d", "", "e", "", "f", ""),
                     rel_heights = c(1, 1, 1),
                     rel_widths = c(0.05,1))
  figdef_list <- list("fig" = fig5def, "leg" = leg)
  figdef_list
}
figdef_list <- plot_certs(cert_runs)

fig5 <- cowplot::plot_grid(figdef_list$leg,
                   cowplot::plot_grid(fig5_left, NA, figdef_list$fig, ncol = 3, rel_widths = c(1, 0.05, 1)),
                   ncol = 1,
                   rel_heights = c(0.2,1))
fig_save("fig5", fig5, width = 12, height = 12)


## ------------------------------------------------
# Fig 5 Numbers
# -----------------------------------------------

deaths_to_date <- cert_runs[[2]] %>% squire::format_output("D") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

active_infections <- squire::format_output(cert_runs[[2]], c("S","R","D"), date_0 = date_0) %>%
  pivot_wider(names_from = compartment, values_from = y) %>%
  mutate(active = sum(ls_out$parameters$population)-D-R-S) %>%
  filter(t == 0) %>% select(active) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

daily_infections <- cert_runs[[2]] %>% squire::format_output("infections") %>% filter(t==0) %>%
  select(y) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

attack_rate <- format_output(cert_runs[[2]], "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(cert_runs[[3]]$parameters$population) - median(y))/sum(cert_runs[[3]]$parameters$population)) %>%
  na.omit %>% filter(t == 0) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))

admissions <- ls_out %>% squire::format_output(c("hospital_incidence","ICU_incidence"), date_0 = date_0) %>%
  group_by(t,replicate, date) %>% summarise(y=sum(y,na.rm=TRUE)) %>%
  ungroup %>% group_by(t, date) %>% summarise(ymed=median(y,na.rm=TRUE), ymin=quantile(y, 0.025), ymax=quantile(y, 0.975))

days_in_2020 <- length(seq.Date(as.Date(date_0), as.Date("2020-12-31"),1))
longer <- squire::projections(ls_out, R0_change = 1, tt_R0 = 0, time_period = days_in_2020)
attack_rate_2021 <- format_output(longer, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(ls_out$parameters$population) - median(y))/sum(ls_out$parameters$population)) %>%
  na.omit %>% filter(t == days_in_2020) %>% ungroup() %>% select(ar) %>% unlist %>% quantile(c(0.025, 0.5, 0.975))


## ------------------------------------------------
# SUPPLEMENTARY
# ------------------------------------------------

## --------------------------------------------
# Supp Figure: sensitivity to Age and Beds
## --------------------------------------------

supp_sens_1a <- ll %>% filter(reporting_fraction %in% c(0.01,0.0125,0.015,0.02)) %>%
  mutate(city_age = c("Older Population","Same as Syria")[match(city_age, c("older","same"))]) %>%
  mutate(reporting_fraction_factor = paste(reporting_fraction_factor, "Reporting")) %>%
  ggplot(aes(x = beds, y = ll_reported, color = poorer_health_outcomes)) +
  geom_point() + geom_smooth(se = FALSE, span = 0.9) +
  ylab("Log Likelihood with 32 deaths/day baseline mortality\n") +
  xlab("\nAssumed Hopsital Beds Available") +
  theme_bw() +
  ggpubr::theme_pubclean() +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        strip.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill="white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted")) +
  scale_color_viridis_d(end = 0.8, name = "Poorer Health Outcomes:") +
  facet_grid(reporting_fraction_factor~city_age, scales = "free")

supp_sens_1b <- ll %>% filter(reporting_fraction %in% c(0.01,0.0125,0.015,0.02)) %>%
  mutate(city_age = c("Older Population","Same as Syria")[match(city_age, c("older","same"))]) %>%
  mutate(reporting_fraction_factor = paste(reporting_fraction_factor, "Reporting")) %>%
  ggplot(aes(x = beds, y = ll_extra, color = poorer_health_outcomes)) +
  geom_point() + geom_smooth(se = FALSE, span = 0.9) +
  ylab("Log Likelihood with 64 deaths/day baseline mortality\n") +
  xlab("\nAssumed Hopsital Beds Available") +
  theme_bw() +
  ggpubr::theme_pubclean() +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        strip.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill="white"),
        panel.grid.major.x = element_line(color = "grey", linetype = "dotted")) +
  scale_color_viridis_d(end = 0.8, name = "Poorer Health Outcomes:") +
  facet_grid(reporting_fraction_factor~city_age, scales = "free")

supp_sens <- cowplot::plot_grid(supp_sens_1a, NA, supp_sens_1b, labels = c("a", "", "b"), rel_widths = c(1,0.1,1), ncol = 3)
fig_save(name = "supp_sens", supp_sens, width = 8, height = 8)

## --------------------------------------------
# Rt analysis and Second Wave
## --------------------------------------------

rt_plot_immunity <- function(out) {

  get_immunity_ratios <- function(out) {

    mixing_matrix <- squire:::process_contact_matrix_scaled_age(
      out$pmcmc_results$inputs$model_params$contact_matrix_set[[1]],
      out$pmcmc_results$inputs$model_params$population
    )

    dur_ICase <- out$parameters$dur_ICase
    dur_IMild <- out$parameters$dur_IMild
    prob_hosp <- out$parameters$prob_hosp

    # assertions
    squire:::assert_single_pos(dur_ICase, zero_allowed = FALSE)
    squire:::assert_single_pos(dur_IMild, zero_allowed = FALSE)
    squire:::assert_numeric(prob_hosp)
    squire:::assert_numeric(mixing_matrix)
    squire:::assert_square_matrix(mixing_matrix)
    squire:::assert_same_length(mixing_matrix[,1], prob_hosp)

    if(sum(is.na(prob_hosp)) > 0) {
      stop("prob_hosp must not contain NAs")
    }

    if(sum(is.na(mixing_matrix)) > 0) {
      stop("mixing_matrix must not contain NAs")
    }

    index <- squire:::odin_index(out$model)
    pop <- out$parameters$population
    t_now <- which(as.Date(rownames(out$output)) == max(out$pmcmc_results$inputs$data$date))
    prop_susc <- lapply(seq_len(dim(out$output)[3]), function(x) {
      t(t(out$output[seq_len(t_now), index$S, x])/pop)
    } )

    relative_R0_by_age <- prob_hosp*dur_ICase + (1-prob_hosp)*dur_IMild

    adjusted_eigens <- lapply(prop_susc, function(x) {

      unlist(lapply(seq_len(nrow(x)), function(y) {
        if(any(is.na(x[y,]))) {
          return(NA)
        } else {
          Re(eigen(mixing_matrix*x[y,]*relative_R0_by_age)$values[1])
        }
      }))

    })

    betas <- lapply(out$replicate_parameters$R0, function(x) {
      squire:::beta_est(squire_model = out$pmcmc_results$inputs$squire_model,
                        model_params = out$pmcmc_results$inputs$model_params,
                        R0 = x)
    })

    ratios <- lapply(seq_along(betas), function(x) {
      (betas[[x]] * adjusted_eigens[[x]]) / out$replicate_parameters$R0[[x]]
    })

    return(ratios)
  }

  if (is.null(out$parameters$country)) {
    iso3c <- "NA"
  } else {
    iso3c <- squire::get_population(out$parameters$country)$iso3c[1]
  }

  if("pmcmc_results" %in% names(out)) {
    wh <- "pmcmc_results"
  } else {
    wh <- "scan_results"
  }

  date <- max(as.Date(out$pmcmc_results$inputs$data$date))
  date_0 <- date

  # impact of immunity ratios
  ratios <- get_immunity_ratios(out)

  # create the Rt data frame
  rts <- lapply(seq_len(length(out$replicate_parameters$R0)), function(y) {

    tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                               change = out$interventions$R0_change,
                                               start_date = out$replicate_parameters$start_date[y],
                                               steps_per_day = 1/out$parameters$dt)

    if(wh == "scan_results") {
      Rt <- c(out$replicate_parameters$R0[y],
              vapply(tt$change, out[[wh]]$inputs$Rt_func, numeric(1),
                     R0 = out$replicate_parameters$R0[y], Meff = out$replicate_parameters$Meff[y]))
    } else {
      Rt <- squire:::evaluate_Rt_pmcmc(
        R0_change = tt$change,
        date_R0_change = tt$dates,
        R0 = out$replicate_parameters$R0[y],
        pars = as.list(out$replicate_parameters[y,]),
        Rt_args = out$pmcmc_results$inputs$Rt_args)
    }

    df <- data.frame(
      "Rt" = Rt,
      "Reff" = Rt*tail(na.omit(ratios[[y]]),length(Rt)),
      "R0" = na.omit(Rt)[1]*tail(na.omit(ratios[[y]]),length(Rt)),
      "date" = tt$dates,
      "iso" = iso3c,
      rep = y,
      stringsAsFactors = FALSE)
    df$pos <- seq_len(nrow(df))
    return(df)
  } )

  rt <- do.call(rbind, rts)
  rt$date <- as.Date(rt$date)

  rt <- rt[,c(5,4,1,2,3,6,7)]

  new_rt_all <- rt %>%
    group_by(iso, rep) %>%
    arrange(date) %>%
    complete(date = seq.Date(min(rt$date), date_0, by = "days"))

  column_names <- colnames(new_rt_all)[-c(1,2,3)]
  new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("down"))
  new_rt_all <- fill(new_rt_all, all_of(column_names), .direction = c("up"))

  suppressMessages(sum_rt <- group_by(new_rt_all, iso, date) %>%
                     summarise(Rt_min = quantile(Rt, 0.025),
                               Rt_q25 = quantile(Rt, 0.25),
                               Rt_q75 = quantile(Rt, 0.75),
                               Rt_max = quantile(Rt, 0.975),
                               Rt_median = median(Rt),
                               Rt = mean(Rt),
                               R0_min = quantile(R0, 0.025),
                               R0_q25 = quantile(R0, 0.25),
                               R0_q75 = quantile(R0, 0.75),
                               R0_max = quantile(R0, 0.975),
                               R0_median = median(R0),
                               R0 = mean(R0),
                               Reff_min = quantile(Reff, 0.025),
                               Reff_q25 = quantile(Reff, 0.25),
                               Reff_q75 = quantile(Reff, 0.75),
                               Reff_max = quantile(Reff, 0.975),
                               Reff_median = median(Reff),
                               Reff = mean(Reff)))

  min_date <- min(as.Date(out$replicate_parameters$start_date))

  country_plot <- function(vjust = -1.2) {
    ggplot(sum_rt %>% filter(
      date > min_date & date <= as.Date(as.character(date_0+as.numeric(lubridate::wday(date_0)))))) +
      geom_ribbon(mapping = aes(x=date, ymin=R0_min, ymax = R0_max, group = iso), fill = "#8cbbca") +
      geom_ribbon(mapping = aes(x = date, ymin = R0_q25, ymax = R0_q75, group = iso), fill = "#3f8da7") +
      geom_ribbon(mapping = aes(x=date, ymin=Reff_min, ymax = Reff_max, group = iso), fill = "#96c4aa") +
      geom_ribbon(mapping = aes(x = date, ymin = Reff_q25, ymax = Reff_q75, group = iso), fill = "#48996b") +
      geom_line(mapping = aes(x = date, y = Reff_median), color = "#48996b") +
      geom_hline(yintercept = 1, linetype = "dashed") +
      geom_hline(yintercept = sum_rt$R0_median[1], linetype = "dashed") +
      theme_bw() +
      theme(axis.text = element_text(size=12)) +
      xlab("") +
      ylab("Reff") +
      scale_x_date(breaks = "2 weeks",
                   limits = as.Date(c(as.character(min_date),
                                      as.character(date_0+as.numeric(lubridate::wday(date_0))))),
                   date_labels = "%d %b",
                   expand = c(0,0)) +
      theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")
      )
  }


  res <- list("plot" = suppressWarnings(country_plot()), "rts" = sum_rt)
  return(res)
}

rtp <- rt_plot_immunity(maintain_4months)

# run one using full reporting and default parameters
# id <- orderly::orderly_run("syria_under_reporting", parameters = list(
#   date = "2020-08-27", reporting_fraction=1, poorer_health_outcomes = FALSE,
#   city_age = 2079000, hospital_normal_use = 0.55, hosp_beds = 4800, late_start=FALSE
#   ))
# # id <- "20200828-153112-4f91e850"
# maintain_rf_1 <- readRDS(file.path(here::here(), "draft/syria_under_reporting", id, "res.rds"))
# saveRDS(maintain_rf_1, file.path(here::here(), "analysis/data/derived/02_09/maintain_rf.rds"))

maintain_rf_1 <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/maintain_rf.rds"))
rtp_rf_1 <- rt_plot_immunity(maintain_rf_1)

supp_rtp <- cowplot::plot_grid(rtp$plot, rtp_rf_1$plot, ncol = 1, labels = "auto")
fig_save(name = "supp_reff", supp_rtp, width = 8, height = 6)

# For Rt values to be reported
rts_now <- rtp$rts %>% filter(date == date_0)

# Rt values likely for new maximum Rt
rt_likely_r0 <- rtp$rts %>% filter(date == "2020-03-19")

# Relax scenario
return_4_months <- squire::projections(maintain_4months, time_period = 120, R0 = 2, tt_R0 = 0)
second_wave_fig <- overview_plot(return_4_months, date_0)
fig_save("supp_second_wave", second_wave_fig, width = 10, height = 8)

## --------------------------------------------
# Alternative attack rate
## --------------------------------------------

ars_default <- squire::format_output(maintain_4months, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(maintain_4months$parameters$population) - median(y))/sum(maintain_4months$parameters$population)) %>%
  na.omit %>% filter(t == 0)
ars_default$pho <- FALSE

pho <- readRDS(ll$file[(ll$dam_pop==2394000 & ll$hosp_beds==4300 &
                                  ll$reporting_fraction == 0.0125 &
                                  ll$poorer_health_outcomes == TRUE &
                                  ll$hospital_normal_use == 0.55 &
                                  ll$city_age == "same")])

#saveRDS(pho, file.path(here::here(), "analysis/data/derived/02_09/pho.rds"))
pho <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/pho.rds"))

ars_alternative <- squire::format_output(pho, "S", date_0 = date_0) %>%
  group_by(t, replicate, compartment, date) %>%
  summarise(ar = (sum(maintain_4months$parameters$population) - median(y))/sum(maintain_4months$parameters$population)) %>%
  na.omit %>% filter(t == 0)
ars_alternative$pho <- TRUE

ars_to_plot <- rbind(ars_default, ars_alternative)
ar_pho <- ggpubr::ggdensity(ars_to_plot, x = "ar", fill = "pho", color = "pho",
                            add = "median", rug = TRUE, y = "..scaled..") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        strip.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill="white")) +
  scale_fill_viridis_d(end = 0.8, name = "Poorer Health Outcomes:")  +
  scale_color_viridis_d(end = 0.8, name = "Poorer Health Outcomes:")  +
  ylab("Scaled Density") +
  xlab("Attack Rate")
fig_save("supp_ars", ar_pho, width = 7, height = 4)

## --------------------------------------------
# Case Data and Trends
## --------------------------------------------

daily_infections <- maintain_4months %>% squire::format_output("infections", date_0 = date_0) %>%
  group_by(t, date) %>% summarise(y=median(y,na.rm=TRUE)) %>% rename(predicted = y)

admissions <- maintain_4months %>% squire::format_output(c("hospital_incidence","ICU_incidence"), date_0 = date_0) %>%
  group_by(t,replicate, date) %>% summarise(y=sum(y,na.rm=TRUE)) %>%
  ungroup %>% group_by(t, date) %>% summarise(ymed=median(y,na.rm=TRUE), ymin=quantile(y, 0.025), ymax=quantile(y, 0.975)) %>%
  rename_at(.funs = (function(x){paste0("admissions_",x)}), .vars = vars(ymed:ymax))

icu <- maintain_4months %>% squire::format_output(c("ICU_incidence"), date_0 = date_0) %>%
  group_by(t,replicate, date) %>% summarise(y=sum(y,na.rm=TRUE)) %>%
  ungroup %>% group_by(t, date) %>% summarise(ymed=median(y,na.rm=TRUE), ymin=quantile(y, 0.025), ymax=quantile(y, 0.975)) %>%
  rename_at(.funs = (function(x){paste0("admissions_",x)}), .vars = vars(ymed:ymax))

daily_infections <- left_join(daily_infections, admissions, by = c("t", "date"))

dam_cases <- readxl::read_xlsx(file.path(here::here(), "analysis/data/raw/dam_daily_inc.xlsx"))
dam_cases$date <- as.Date(dam_cases$date,"%d %b %Y")
daily_infections$reported <- dam_cases$cases[match(daily_infections$date, dam_cases$date)]
daily_infections$date <- as.Date(daily_infections$date)
daily_infections <- daily_infections %>% filter(date <= date_0)

max_predicted <- max(daily_infections$predicted, na.rm = TRUE)
max_admissions <- max(daily_infections$admissions_ymed, na.rm = TRUE)
max_reported <- max(daily_infections$reported, na.rm = TRUE)

daily_infections$weekly <- zoo::rollmean(daily_infections$reported, 7, na.pad = TRUE)
reported <- daily_infections$reported
reported[is.na(reported)] <- 0
daily_infections$cumualtive_reported <- cumsum(reported)

## testing data
testing <- readxl::read_xlsx(file.path(here::here(), "analysis/data/raw/testing.xlsx"))
testing$Damascus <- zoo::na.approx(testing$Damascus)
testing$date <- as.Date(testing$Date)
testing <- testing %>% filter(Damascus > 0)
testing$diff_dam <- rev(c(rev(testing$Damascus)[1],diff(rev(testing$Damascus))))

# Plot
supp_testing <-ggplot(daily_infections, aes(x = date, y = reported)) +
  geom_bar(aes(date, Damascus/max(Damascus)*max_reported*0.9), data = testing, stat = "identity", alpha = 0.25) +
  geom_line(aes(x = date, y = (predicted/max_predicted)*max_reported), inherit.aes = FALSE, lwd = 1.5, color = "#CA3433") +
  geom_point() +
  geom_line(aes(date,weekly)) +
  # geom_line(aes(x = date, y = (admissions_ymed/max_admissions)*max_reported), inherit.aes = FALSE, lwd = 1.5, color = "#337eca") +
  geom_text(aes(date, (Damascus/max(Damascus)*max_reported*0.9)+1.2, label = round(Damascus)), data = testing) +
  ylab("Daily Reported Case\n") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ggpubr::theme_pubclean(base_size = 14) +
  #scale_y_continuous(sec.axis = sec_axis( trans=~.*max_admissions/max_reported, name="Model Predicted Hospital Admissions\n"), expand = c(0,1)) +
  scale_y_continuous(sec.axis = sec_axis( trans=~.*max_predicted/max_reported, name="Model Predicted Infections\n"), expand = c(0,1)) +
  theme(axis.line = element_line(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())
fig_save("supp_testing", supp_testing, 12, 8)

## --------------------------------------------
## BRT Proof
## --------------------------------------------

brt <- readRDS(file.path(here::here(), "analysis/data/derived/02_09/brt.rds"))
mobs <- lapply(c("TUR","IRQ","JOR","LBN","ISR", "SYR"), function(x) {
  ggplot(brt[[x]] %>% filter(date <= as.Date("2020-08-27")), aes(date, C_predict/max(C_predict))) +
    geom_point(aes(date, C/C[1])) +
    geom_step(color = "#CA3433", lwd = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = scales::percent, breaks = c(.2,.4,.6,.8,1)) +
    xlab("") + ylab("Mobility") + theme_bw() +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line()) +
    ggtitle(countrycode::countrycode(x, "iso3c", "country.name.en")) +
    geom_hline(yintercept = 0.1, lwd = 0)
  })

supp_mob <- cowplot::plot_grid(plotlist = mobs, ncol = 2)
fig_save("supp_mobility", supp_mob, width = 8, height = 8)

