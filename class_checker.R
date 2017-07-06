library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(mailR)
library(xtable)

if(file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

paste(Sys.time(), "Checking for new classes")

TIME_PATTERN <- "([1]{0,1}\\d:\\d{2} [AP]M - \\d{1,2}:\\d{2} [AP]M)"
BOAT_PATTERN <- "^([A-Z][A-z ]+)([A-Z].*)$"
CLASS_TYPES <- c("Cruise", "Maintenance Sail",
                 "Orientation", "Refresher", "TillerTime")
CHANGE_VARS <- c("Seats taken", "Status")

if(file.exists("classes.Rda") == TRUE) {
 load("classes.Rda")
  old_classes <- new_classes
  rm(new_classes)
} else {
  old_classes <- NULL
}

url       <- "https://my.sailchicago.org/Account/Login"
pgsession <- html_session(url)
pgform    <- html_form(pgsession)[[1]]

filled_form <- set_values(pgform,
                          "UserName" = Sys.getenv("SC_USER"),
                          "Password" = Sys.getenv("SC_PASS"))
submit_form(pgsession, filled_form)
seat_reservation <- jump_to(pgsession,
                            "https://my.sailchicago.org/SeatReservation/SeatAvailability")
page <- read_html(seat_reservation)
class_table <- html_nodes(x = page, xpath = '//*[@id="content_inner_wrapper"]/div/div/table')
classes0 <- html_table(class_table)[[1]]

new_classes <- classes0 %>%
  transmute(
    temp_header  = X1 %in% CLASS_TYPES,
    `Type`       = ifelse(temp_header, X1, NA),
    `Date`       = as.Date(str_replace(X1, TIME_PATTERN, ""), "%A, %B %d, %Y"),
    `Day`        = as.character(`Date`, format = "%A"),
    `Date`       = as.character(`Date`),
    `Times`      = sapply(X1, function(x) str_match(x, TIME_PATTERN)[2]),
    `Boat type`  = sapply(X2, function(x) str_match(x, BOAT_PATTERN)[3]),
    `Boat name`  = sapply(X2, function(x) str_match(x, BOAT_PATTERN)[2]),
    `Seats taken`= as.character(str_count(X3, "\r\n") + 1),
    `Status`     = X4
  ) %>%
  fill(Type) %>%
  filter(temp_header != TRUE) %>%
  select(-starts_with("temp"))

paste(Sys.time())
print(new_classes)

if(is.null(old_classes)) {
  old_classes <- new_classes[0,]
}

join_vars <- setdiff(names(new_classes), CHANGE_VARS)
to_email0 <- new_classes %>%
  full_join(old_classes, by = join_vars, suffix=c(".new", ".old")) %>%
  mutate(Change = FALSE)

detect_change <- function(var, df = to_email0) {
  var.new <- paste0(var, ".new")
  var.old <- paste0(var, ".old")

  df[var] <- ifelse(df[[var.old]] != df[[var.new]] |
                      is.na(df[[var.old]]) |
                      is.na(df[[var.new]]),
                    paste0("<font color='red'>",
                           df[[var.old]], " &#8594; ", df[[var.new]],
                           "</font>"),
                    df[[var.new]])

  df[var.old] <- df[var.new] <- NULL
  return(df)
}
for(var in CHANGE_VARS) {
  to_email0 <- detect_change(var)
  to_email0$Change <- grepl("font color", to_email0[[var]]) | to_email0$Change
}

to_email <- to_email0 %>%
  filter(Type == "TillerTime") %>%
  filter(Change == TRUE) %>%
  select(-Change)

if(nrow(to_email) > 0) {
  paste(Sys.time(), "Sending email")
  signup_link <- paste0("<a href='",
                        "https://my.sailchicago.org/SeatReservation/SeatAvailability",
                        "'>Sign up</a>")
  email_body <- paste0(
    print(xtable(to_email),
          type="html",
          sanitize.text.function=function(x){x},
          include.rownames=FALSE),
    "<br><br>",
    signup_link)
  send.mail(from = Sys.getenv("EMAIL_FROM"),
            to = strsplit(Sys.getenv("CC_RECIPIENTS"), split=",")[[1]],
            subject = "Changes to Sail Chicago Classes",
            body = email_body,
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = Sys.getenv("EMAIL_FROM"),
                        passwd = Sys.getenv("EMAIL_PASS"), ssl = TRUE),
            authenticate = TRUE,
            html = TRUE,
            send = TRUE)
} else {
  paste(Sys.time(), "Nothing to send")
}

save(new_classes, file="classes.Rda")
rm(list = ls())


