library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(mailR)
library(xtable)

paste(Sys.time(), "Checking for new classes")

TIME_PATTERN <- "([1]{0,1}\\d:\\d{2} [AP]M - \\d{1,2}:\\d{2} [AP]M)"
BOAT_PATTERN <- "^([A-Z][A-z ]+)([A-Z].*)$"
CLASS_TYPES <- c("Cruise", "Maintenance Sail",
                 "Orientation", "Refresher", "TillerTime")
JOIN_VARS <- c("Type", "Date", "Times", "Boat type", "Boat name")

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
    `Date`       = as.character(`Date`, format = "%a %b %d"),
    `Times`      = sapply(X1, function(x) str_match(x, TIME_PATTERN)[2]),
    `Boat type`  = sapply(X2, function(x) str_match(x, BOAT_PATTERN)[3]),
    `Boat name`  = sapply(X2, function(x) str_match(x, BOAT_PATTERN)[2]),
    `Seats taken`= str_count(X3, "\r\n") + 1,
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

to_email <- new_classes %>%
  full_join(select(old_classes, -`Seats taken`), by = JOIN_VARS) %>%
  mutate(
    Change  = Status.x != Status.y,
    Change  = ifelse(is.na(Change), TRUE, Change)
  ) %>%
  filter(Type == "TillerTime") %>%
  filter(Change == TRUE) %>%
  select(-Status.y, -Change) %>%
  rename(Status = Status.x)

if(nrow(to_email) > 0) {
  paste(Sys.time(), "Sending email")
  signup_link <- paste0("<a href='",
                        "https://my.sailchicago.org/SeatReservation/SeatAvailability",
                        "'>Sign up</a>")
  email_body <- paste0(print(xtable(to_email), type="html"),
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


