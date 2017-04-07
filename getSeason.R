#http://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to

## Define function to assign records to a season
getSeason <- function(DATES) {
 Wi <- as.Date("2012-06-01", format = "%Y-%m-%d") # Winter 
 Sp <- as.Date("2012-09-01",  format = "%Y-%m-%d") # Spring 
 Su <- as.Date("2012-12-01",  format = "%Y-%m-%d") # Summer 
 Au <- as.Date("2012-04-01",  format = "%Y-%m-%d") # Autumn 
 
 # Convert dates from any year to 2012 dates
 d <- as.Date(strftime(DATES, format="2012-%m-%d"))
 
 ifelse (d >= Wi & d < Sp, "Winter",
         ifelse (d >= Sp & d < Su, "Spring",
                 ifelse (d >= Su | d < Au, "Summer", "Autumn")))
}


#datain$season <- getSeason(datain$pred.date)