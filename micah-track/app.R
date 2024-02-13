library(shiny)
library(dplyr)
library(googlesheets4)
library(carutools)
library(janitor)
library(lubridate)
library(stringr)
library(scales)


sheet_id <- "1YnH5BCPb4jPekyGIIur5DTrhcb8U_7dzQB22vSHoVSQ"
true_pin <- readLines("secrets/pin")

gs4_deauth()
gs4_auth(email = TRUE, scopes = "spreadsheets.readonly",
         cache = "secrets")

feeds <- read_sheet(sheet_id)

feeds <- clean_names(feeds)
feeds <- rename(feeds,
                start_time = what_time_did_the_feed_start,
                end_time   = what_time_did_the_feed_end,
                boob       = which_boob,
                comments   = other_comments)

fix_time <- function(entry, time){

  time <-
    as.character(time) |>
    str_extract("\\d\\d:\\d\\d:\\d\\d") |>
    hms()

  day <-
    str_extract(entry, "\\d{4}-\\d\\d-\\d\\d") |>
    ymd()

  time <- day + time

  future <- time > entry

  time[future] <- time[future] - days(1)

  time

}

feeds <- mutate(feeds,
                across(c(start_time, end_time),
                       \(x) fix_time(timestamp, x)))

feeds <- mutate(feeds, boob = factor(boob))

feeds <- mutate(feeds, duration = end_time - start_time)

feeds <- tally_delimited_string(feeds, poo_wee, names_prefix = "")

twentyfour <- filter(feeds, start_time > Sys.time() - days(1))

last <-
  filter(feeds, boob %in% c("Left", "Right", "Both")) |>
  filter(start_time == max(start_time)) |>
  head(1)

ui <- fluidPage(
  titlePanel("Micah's feeds:"),

  p("Our precious boy has fed",
    strong(textOutput("feeds_today", inline = TRUE)),
    "times in the last 24 hours."),

  p("He last fed ",
    strong(textOutput("time_since_last_feed", inline = TRUE)),
    "ago for about",
    strong(textOutput("duration_of_last_feed", inline = TRUE)), "minutes on ",
    strong(textOutput("which_boob", inline = TRUE)), ".")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$feeds_today <- renderText(nrow(twentyfour))
  output$duration_of_last_feed <- renderText(as.numeric(last[["duration"]]))

  output$time_since_last_feed <- renderText({

    timespan <- label_timespan("hours", accuracy = 0.1)

    last_feed <- last[["end_time"]]
    (Sys.time() - last_feed) |>
      timespan()
  })

  output$which_boob <- renderText({
    last_boob <- last[["boob"]]

    case_match(last_boob,
               "Left"  ~ "the left boob",
               "Right" ~ "the right boob",
               "Both"  ~ "both boobs"
               )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
