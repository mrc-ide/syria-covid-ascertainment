### Extra wafiat work

## all
time <- xml2::read_html(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_all.html"))
interest <- rvest::html_nodes(time, "[class='oajrlxb2 g5ia77u1 qu0x051f esr5mh6w e9989ue4 r7d6kgcz rq0escxv nhd2j8a9 a8c37x1j p7hjln8o kvgmc6g5 cxmmr5t8 bi6gxh9e hcukyx3x jb3vyjys rz4wbd8a qt6c0cv9 a8nywdso i1ao9s8h esuyzwwr f1sip0of lzcic4wl gmql0nx0 gpro0wi8 gq8dxoea lyjsgrqv']") # 08_09

hrefs <- rvest::html_attr(interest, "href")
srcs <- pbapply::pbsapply(interest, function(x) {rvest::html_attr(rvest::html_nodes(x, "img"), "src")[1]})
srcs <- gsub("./wafiat_all_files/","",unlist(srcs), fixed = TRUE)

# have to go via selenium as Facebook changed their privacy
get_times <- function(x) {

  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "firefox"
  )
  remDr$open()

  times <- c()
  for(i in seq_along(x)){
    message(i)
    remDr$navigate(hrefs[x[i]])
    ps <- remDr$getPageSource()
    if(grepl("s ago|Today at", ps[[1]])) {
      times[i] <- as.Date("2021-01-14")
    } else if (grepl("Yesterday", ps[[1]])) {
      times[i] <- as.Date("2021-01-13")
    } else if (grepl("Tuesday at", ps[[1]])) {
      times[i] <- as.Date("2021-01-12")
    } else if (grepl("Monday at", ps[[1]])) {
      times[i] <- as.Date("2021-01-11")
    } else if (grepl("Jan at|Jan<", ps[[1]])) {
      time <- paste0(gsub("(.*)(\\d{2} Jan)(.*)", "\\2", ps[[1]]), " 2021")
      if(nchar(time)>15) {
        time <- paste0(gsub("(.*)(\\d{1} Jan)(.*)", "\\2", ps[[1]]), " 2021")
      }
      times[i] <- as.Date(time, "%d %B %Y")
    } else {
      time <- gsub("(.*)(\\d{2} \\w\\w\\w \\d\\d\\d\\d)(.*)", "\\2", ps[[1]])
      if(nchar(time)>15) {
        time <- gsub("(.*)(\\d{1} \\w\\w\\w \\d\\d\\d\\d)(.*)", "\\2", ps[[1]])
      }
      times[i] <- as.Date(time, "%d %B %Y")
    }
  }

  return(times)
}

# Ran in jobs view for 1st 2163 to arrive at date from last wafiat extraction
times_l1 <- get_times(1:200)
times_l2 <- get_times(201:400)
times_l3 <- get_times(401:600)
times_l4 <- get_times(601:800)
times_l5 <- get_times(801:1000)
times_l6 <- get_times(1001:1200)
times_l7 <- get_times(1201:1400)
times_l8 <- get_times(1401:1600)
times_l9 <- get_times(1601:1800)
times_l10 <- get_times(1801:2000)
times_l11 <- get_times(2001:2163)

times <- as.Date(c(times_l1,times_l2, times_l3, times_l4, times_l5, times_l6,
                   times_l7, times_l8, times_l9, times_l10, times_l11),
                 lubridate::origin)

# turn into df
df_t <- data.frame("date"=times)
df_t$date <- as.Date(df_t$date, lubridate::origin)
df_t$year_month <- paste0(lubridate::year(df_t$date), "-", vapply(lubridate::month(df_t$date), tchr_zer, character(1)))
df_t$href <- hrefs[1:2163]
df_t$src <- srcs[1:2163]
saveRDS(df_t, file.path(here::here(), "analysis/wafiat_analysis/14_01/wafit_extra_timeline_dates.rds"))

## copy the pictures over for google analysis
# fl_mtchs <- match(srcs[1:2163], list.files(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_all_files/")))
# to_copy <- list.files(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_all_files/"), full.names = TRUE)[fl_mtchs]
# file.copy(to_copy, "/home/oj/GoogleDrive/AcademicWork/covid/documents/Syria/wafiat_new_share/", overwrite = FALSE)

## get labelling from google
# library(googlesheets4)
# goog_vapi <- read_sheet("https://docs.google.com/spreadsheets/d/1I2bT1Zgwx5d4l1ohMPSCwbd_JWfuJXUmYKL-FuitOUk/edit?ts=5f57690b#gid=957771377", sheet = 3)
# saveRDS(goog_vapi, file.path(here::here(), "analysis/wafiat_analysis/14_01/goog_vapi.rds"))
# images_uploaded <- list.files("/home/oj/GoogleDrive/AcademicWork/covid/documents/Syria/wafiat_new_share/")
goog_vapi <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/14_01/goog_vapi.rds"))

# load the dates
df_t_revision <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafit_extra_timeline_dates.rds"))
df_t_revision$source <- "Revision Photos"
df_t_revision$year_month_dated <- as.Date(as.character(vapply(df_t_revision$year_month, paste0, character(1), "-01", collapse = "")))

# join the two to subset to certificates
df_t_revision$labels <- goog_vapi$Lables[match(df_t_revision$src, goog_vapi$Name)]
df_t_revision$is_document <- goog_vapi$`Is document`[match(df_t_revision$src, goog_vapi$Name)]
df_t_revision$is_text <- goog_vapi$`is text`[match(df_t_revision$src, goog_vapi$Name)]
df_t_revision$manual_remove <-  goog_vapi$`manual remove`[match(df_t_revision$src, goog_vapi$Name)]
df_t_revision$certificate <- ((df_t_revision$is_document == "1" | df_t_revision$is_text == "1") & is.na(df_t_revision$manual_remove))
df_t_revision <- df_t_revision[order(df_t_revision$date),]

# check for duplicate images in batches of 2000 (not enough memory to do all at once but 2000 should catch duplicats)
img_paths_revision <- file.path("analysis/wafiat_analysis/14_01/wafiat_all_files/", df_t_revision$src)
imgs <- c(img_paths_revision[which(df_t_revision$source == "Revision Photos")])
imgs <- grep("jpg", imgs, value = TRUE)

jpgs <- lapply(imgs, function(x) {as.raster(jpeg::readJPEG(x))})
mat <- lower.tri(matrix(FALSE, length(jpgs), length(jpgs)))
for(j in seq_len(length(jpgs)-1)) {
  for(k in (j+1):length(jpgs)) {
    mat[k,j] <- identical(jpgs[j], jpgs[k])
  }

}
issues <- which(mat, arr.ind = TRUE)
issues <- issues[seq(1,nrow(issues),2),1]
to_remove <- imgs[issues]
get_rid <- to_remove
df_t_revision <- df_t_revision[-which(df_t_revision$src %in% basename(get_rid)),]

# save the results
saveRDS(df_t_revision, file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_df_revision.rds"))

# Bundle all together
df_t <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_df.rds"))
df_t_revision <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_df_revision.rds"))
df_t <- rbind(df_t, df_t_revision)

# remove duplicate image names
df_t <- df_t[-which(duplicated(df_t$src)), ]
saveRDS(df_t, file.path(here::here(), "analysis/wafiat_analysis/14_01/wafiat_df_all.rds"))

# Store data
write.csv(df_t, file.path(here::here(), "analysis/data/raw/supp_table_7_revised.csv"))
