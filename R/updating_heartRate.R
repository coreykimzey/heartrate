#updating function

updating_heartRate <- function(){

gs_url("https://docs.google.com/spreadsheets/d/1_-0PuPB-YRDVx7AfOQfMQtKVYQ0CyCMDtLLIOW0h0es/edit?usp=sharing") %>%
gs_read() %>% return()
}
