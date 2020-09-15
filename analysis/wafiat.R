
## Timeline
time <- xml2::read_html(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_timeline_html.html"))
interest <- rvest::html_nodes(time, "div._2eea")
hrefs <- pbapply::pbsapply(interest, function(x) {grep("photos/a", rvest::html_attr(rvest::html_nodes(x, "a"), "href"),value=TRUE)})
srcs <- pbapply::pbsapply(interest, function(x) {rvest::html_attr(rvest::html_nodes(x, "img"), "src")[1]})
srcs <- gsub("./wafiat_timeline_html_files/","",unlist(srcs), fixed = TRUE)

rgs <- magenta:::ranges(50, length(hrefs))
rgs[[length(rgs)+1]] <- seq(tail(rgs,1)[[1]][1], length(interest))
temps <- paste0(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_timeline_urls", paste0(srcs,".html")))
dir.create(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_timeline_urls"))

for(i in seq_along(rgs)){
  message(i)
  download.file(hrefs[rgs[[i]]], temps[rgs[[i]]],quiet=TRUE, method = "libcurl")
  Sys.sleep(5)
}


time_from_url <- function(file = NULL, url = NULL) {

  if (is.null(file)) {
    tf <- tempfile()
    download.file(url, tf)
  } else {
    tf <- file
  }
  rl_h <- paste0(readLines(tf), collapse="")
  wh <- qdapRegex::ex_between(rl_h, 'abbr data','data', fixed = TRUE)
  time <- qdapRegex::ex_between(tail(wh[[1]],1),",  ","\"")[[1]]
  time_ret <- strptime(time, "%d %B %Y at %H:%M") + lubridate::hours(8)

  return(time_ret)

}

times <- pbapply::pblapply(temps, time_from_url)

tchr_zer <- function(x) {
  if(nchar(x) == 1) {
    paste0("0", x)
  } else {
    as.character(x)
  }
}


vectim <- unlist(lapply(times, as.Date))
df_t <- data.frame("date"=vectim)
df_t$date <- as.Date(df_t$date, lubridate::origin)
df_t$year_month <- paste0(lubridate::year(df_t$date), "-", vapply(lubridate::month(df_t$date), tchr_zer, character(1)))
df_t$href <- hrefs
df_t$src <- srcs
saveRDS(df_t, file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_timeline_dates.rds"))

## copy the pictures over for google analysis
# fl_mtchs <- match(srcs, list.files(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_timeline_html_files/")))
# to_copy <- list.files(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_timeline_html_files/"), full.names = TRUE)[fl_mtchs]
# file.copy(to_copy, "/home/oj/GoogleDrive/AcademicWork/covid/documents/Syria/wafiat_share/", overwrite = FALSE)

## Mobile --------------------------------------------------------------------

mobile <- xml2::read_html(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_mobile_html.html"))
interest <- rvest::html_nodes(mobile, "div._2eea")
hrefs <- pbapply::pbsapply(interest, function(x) {grep("photos/a", rvest::html_attr(rvest::html_nodes(x, "a"), "href"),value=TRUE)})
srcs <- pbapply::pbsapply(interest, function(x) {rvest::html_attr(rvest::html_nodes(x, "img"), "src")[1]})
srcs <- gsub("./wafiat_mobile_html_files/","",unlist(srcs), fixed = TRUE)

rgs <- magenta:::ranges(50, length(hrefs))
rgs[[length(rgs)+1]] <- seq(tail(rgs,1)[[1]][1], length(interest))
temps <- paste0(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_mobile_urls", paste0(srcs,".html")))
dir.create(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_mobile_urls"))

for(i in seq_along(rgs)){
  message(i)
  download.file(hrefs[rgs[[i]]], temps[rgs[[i]]],quiet=TRUE, method = "libcurl")
  Sys.sleep(5)
}


time_from_url <- function(file = NULL, url = NULL) {

  if (is.null(file)) {
    tf <- tempfile()
    download.file(url, tf)
  } else {
    tf <- file
  }
  rl_h <- paste0(readLines(tf), collapse="")
  wh <- qdapRegex::ex_between(rl_h, 'abbr data','data', fixed = TRUE)
  time <- qdapRegex::ex_between(tail(wh[[1]],1),",  ","\"")[[1]]
  time_ret <- strptime(time, "%d %B %Y at %H:%M") + lubridate::hours(8)

  return(time_ret)

}

times <- pbapply::pblapply(temps, time_from_url)

tchr_zer <- function(x) {
  if(nchar(x) == 1) {
    paste0("0", x)
  } else {
    as.character(x)
  }
}


vectim <- unlist(lapply(times, as.Date))
df_t <- data.frame("date"=vectim)
df_t$date <- as.Date(df_t$date, lubridate::origin)
df_t$year_month <- paste0(lubridate::year(df_t$date), "-", vapply(lubridate::month(df_t$date), tchr_zer, character(1)))
df_t$href <- hrefs
df_t$src <- srcs
saveRDS(df_t, file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_mobile_dates.rds"))

## copy the pictures over for google analysis
# fl_mtchs <- match(srcs, list.files(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_mobile_html_files/")))
# to_copy <- list.files(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_mobile_html_files/"), full.names = TRUE)[fl_mtchs]
# file.copy(to_copy, "/home/oj/GoogleDrive/AcademicWork/covid/documents/Syria/wafiat_share/", overwrite = FALSE)

## -----------------------------------------------------------------------------

# load the vision API processed data
# library(googlesheets4)
# goog_vapi <- read_sheet("https://docs.google.com/spreadsheets/d/1I2bT1Zgwx5d4l1ohMPSCwbd_JWfuJXUmYKL-FuitOUk/edit?ts=5f57690b#gid=957771377", sheet = 2)
# saveRDS(goog_vapi, file.path(here::here(), "analysis/wafiat_analysis/08_09/goog_vapi.rds"))
goog_vapi <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/08_09/goog_vapi.rds"))

# images_uploaded <- list.files("/home/oj/GoogleDrive/AcademicWork/covid/documents/Syria/wafiat_share/")
# saveRDS(images_uploaded, file.path(here::here(), "analysis/wafiat_analysis/08_09/images_uploaded.rds"))
images_uploaded <- file.path(here::here(), "analysis/wafiat_analysis/08_09/images_uploaded.rds")

# load the dates
df_t_timeline <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_timeline_dates.rds"))
df_t_timeline$source <- "Timeline Photos"
df_t <- readRDS(file.path(here::here(), "analysis/wafiat_analysis/08_09/wafit_mobile_dates.rds"))
df_t$source <- "Mobile Uploads"
df_t <- rbind(df_t, df_t_timeline)
df_t$year_month_dated <- as.Date(as.character(vapply(df_t$year_month, paste0, character(1), "-01", collapse = "")))

# join the two to subset to certificates
df_t$labels <- goog_vapi$Lables[match(df_t$src, goog_vapi$Name)]
df_t$is_document <- goog_vapi$`Is document`[match(df_t$src, goog_vapi$Name)]
df_t$is_text <- goog_vapi$`is text`[match(df_t$src, goog_vapi$Name)]
df_t$manual_remove <-  goog_vapi$`manual remove`[match(df_t$src, goog_vapi$Name)]
df_t$certificate <- ((df_t$is_document == "1" | df_t$is_text == "1") & is.na(df_t$manual_remove))
df_t <- df_t[order(df_t$date),]

# check for duplicate images in batches of 2000 (not enough memory to do all at once but 2000 should catch duplicats)
img_paths_mobile <- file.path("analysis/wafiat_analysis/08_09/wafiat_mobile_html_files/",df_t$src)
img_paths_timeline <- file.path("analysis/wafiat_analysis/08_09/wafiat_timeline_html_files/",df_t$src)
imgs <- c(img_paths_mobile[which(df_t$source == "Mobile Uploads")], img_paths_timeline[which(df_t$source == "Timeline Photos")] )
imgs <- grep("jpg", imgs, value = TRUE)

to_remove <- list()
also_remove <- list()

for(i in 1:10) {
  message(i)
  jpgs <- lapply(imgs[((2000*(i-1))+1):min((2000*i),length(imgs))], function(x) {as.raster(jpeg::readJPEG(x))})
  mat <- lower.tri(matrix(FALSE, length(jpgs), length(jpgs)))
  for(j in seq_len(length(jpgs)-1)) {
    for(k in (j+1):length(jpgs)) {
      mat[k,j] <- identical(jpgs[j], jpgs[k])
    }

  }
  issues <- which(mat, arr.ind = TRUE)
  issues <- issues[seq(1,nrow(issues),2),1]
  to_remove[[i]] <- imgs[((2000*(i-1))+1):min((2000*i),length(imgs))][issues]
}

for(i in 1:9) {
  message(i)
  jpgs <- lapply(imgs[((2000*(i-1))+1000):min((2000*i)+1000,length(imgs))], function(x) {as.raster(jpeg::readJPEG(x))})
  mat <- lower.tri(matrix(FALSE, length(jpgs), length(jpgs)))
  for(j in seq_len(length(jpgs)-1)) {
    for(k in (j+1):length(jpgs)) {
      mat[k,j] <- identical(jpgs[j], jpgs[k])
    }

  }
  issues <- which(mat, arr.ind = TRUE)
  issues <- issues[seq(1,nrow(issues),2),1]
  also_remove[[i]] <- imgs[((2000*(i-1))+1000):min((2000*i)+1000,length(imgs))][issues]
}

get_rid <- unique(c(unlist(to_remove), unlist(also_remove)))
df_t <- df_t[-which(df_t$src %in% basename(get_rid)),]

# save the results
saveRDS(df_t, file.path(here::here(), "analysis/wafiat_analysis/08_09/wafiat_df.rds"))
write.csv(df_t, file.path(here::here(), "analysis/data/supp_table_7.csv"))


