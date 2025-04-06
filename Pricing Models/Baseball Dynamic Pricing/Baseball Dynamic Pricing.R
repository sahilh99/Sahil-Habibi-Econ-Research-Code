#Clear objects
rm(list = ls())

#Install package for fonts
install.packages("extrafont")

#Load up libraries
#Create list of packages
packages <- c("dplyr", "stargazer", "ggplot2", "broom", "extrafont","purrr")
#Load them
lapply(packages, library, character.only = TRUE)

#Create GitHub links to loop through for data
years <- 2009:2012
base_url <- "https://raw.githubusercontent.com/sahilh99/Sahil-Habibi-Econ-Research-Code/refs/heads/main/Pricing%20Models/Baseball%20Dynamic%20Pricing/Red%20Sox%20Tickets%20Sold%20Data/red_sox_"

#Load up the data and then combine
all_red_sox_data <- lapply(years, function(year) {
  url <- paste0(base_url, year, ".csv")
  current_data <- read.csv(url) %>% mutate(year = year)
  return(current_data)
}) %>%
  #Combine the data together
  bind_rows()

#Create intervals for dummy variable generations of days before game
intervals <- list("1_5" = c(1, 5), "6_10" = c(6, 10), "11_15" = c(11, 15),
                  "16_20" = c(16, 20), "21_30" = c(21, 30), "31_60" = c(31, 60),
                  "61_90" = c(61, 90), "91_120" = c(91, 120), "121_150" = c(121, 150),
                  "151_200" = c(151, 200), "201_250" = c(201, 250))

#Use the intervals defined above to create the dummies
dummy_data <- map_dfc(intervals, ~ as.integer(between(all_red_sox_data$days_from_transaction_until_game, .x[1], .x[2]))) %>%
  rename_with(~ paste0("dummy_", names(intervals)))

#Attach these dummies to the original dataset
all_red_sox_data <- bind_cols(all_red_sox_data, dummy_data)


#Run OLS Regression
model <- lm(logprice ~ dummy_1_5 + dummy_6_10 + dummy_11_15 + dummy_16_20 +
              dummy_21_30 + dummy_31_60 + dummy_61_90 + dummy_91_120 +
              dummy_121_150 + dummy_151_200, data = all_red_sox_data)

#Output regression results into LaTeX for Overleaf
stargazer(model, type = "latex", title = "Regression Results",
          dep.var.labels = "Log Price",
          covariate.labels = c("1-5 Days", "6-10 Days", "11-15 Days", "16-20 Days",
                               "21-30 Days", "31-60 Days", "61-90 Days", "91-120 Days",
                               "121-150 Days", "151-200 Days"), 
          omit.stat = c("f", "ser"), digits = 3,
          out = "BaseballR.tex")

##Will create a graph of the coefficients for visual digestion

#Grab the coefficients
coef_data <- tidy(model, conf.int = TRUE) %>%
  #Take out the intercept
  filter(term != "(Intercept)") 

#Since coefficients are %/100, let's multiply by 100 for graphing
coef_data <- coef_data %>%
  mutate(estimate = estimate * 100,
         conf.low = conf.low * 100,
         conf.high = conf.high * 100)

#Give the order of the xlabels to ensure proper permutation
coef_data$term <- factor(coef_data$term, levels = c("dummy_1_5", "dummy_6_10", "dummy_11_15",
                                                    "dummy_16_20", "dummy_21_30", "dummy_31_60",
                                                    "dummy_61_90", "dummy_91_120", "dummy_121_150",
                                                    "dummy_151_200"))

#Use ggplot to plot the results
ggplot(coef_data, aes(x = term, y = estimate)) +
  #Increase size of the dots
  geom_point(color = "blue", size = 4) +  
  #Add on confidence intervals to show precision or lack thereof
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "gray") +
  #Connect points together to show relation over time
  geom_line(group = 1, size = 1.2, color = "blue") +  
  #Change font
  theme_minimal(base_family = "Arial") +  
  #Add labels
  labs(title = "% Higher Ticket Price Compared to 201-250 Days Out From Game",
       x = "Time Intervals (Days)",
       y = "Ticket Price Increase (%)") +
  #Change design
  theme(
    plot.title = element_text(hjust = 0.5, family = "Arial", size = 18, face = "bold"), #Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  #45 degree angle for visibility
    axis.text.y = element_text(size = 12),  #Make y-axis more visible
    panel.grid.major = element_line(color = "gray90", size = 0.5),  #Add grid lines
    panel.grid.minor = element_blank()  # Remove minor ones
  )


#Now run separate regressions for each year for year-by-year comparisons
models <- all_red_sox_data %>%
  #Separate by year
  split(.$year) %>%
  #Typical regression equation
  lapply(function(data) lm(logprice ~ dummy_1_5 + dummy_6_10 + dummy_11_15 + dummy_16_20 +
                             dummy_21_30 + dummy_31_60 + dummy_61_90 + dummy_91_120 +
                             dummy_121_150 + dummy_151_200, data = data))

#Output to a .tex file for proper presentation abilities in Overleaf
stargazer(models, type = "latex", title = "Regression Results by Year",
          #Add y-variable labels
          dep.var.labels = "Log Price", column.labels = names(models),
          #Add x-variabel labels
          covariate.labels = c("1-5 Days", "6-10 Days", "11-15 Days", "16-20 Days",
                               "21-30 Days", "31-60 Days", "61-90 Days", "91-120 Days",
                               "121-150 Days", "151-200 Days"), 
          omit.stat = c("f", "ser"), digits = 3,
          #Output the file
          out = "BaseballsR.tex")

##Create a year-by-year comparison plot

#Take coefficients from each regression
coef_data <- lapply(models, tidy, conf.int = TRUE) %>%
  bind_rows(.id = "year") 

#Get rid of intercept
coef_data <- coef_data %>% filter(term != "(Intercept)")

#Multiply by 100 for percent interpretation
coef_data <- coef_data %>%
  mutate(estimate = estimate * 100,
         conf.low = conf.low * 100,
         conf.high = conf.high * 100)

#Ensure proper ordering for x variables
coef_data$term <- factor(coef_data$term, levels = c("dummy_1_5", "dummy_6_10", "dummy_11_15",
                                                    "dummy_16_20", "dummy_21_30", "dummy_31_60",
                                                    "dummy_61_90", "dummy_91_120", "dummy_121_150",
                                                    "dummy_151_200"))

#Create the plot
ggplot(coef_data, aes(x = term, y = estimate, group = year, color = year)) +
  #Smooth the lines slightly
  geom_line(size = 1.2, alpha = 0.8) +  
  #Larger points
  geom_point(size = 4, shape = 21, fill = "white", stroke = 1.2) +  
  #Make confidence bars larger
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, alpha = 0.8) + 
  #Arial Font
  theme_minimal(base_family = "Arial") +
  #Create labels
  labs(title = "% Higher Ticket Price Compared to 201-250 Days Out From Game",
       x = "Time Intervals (Days)",
       y = "Ticket Price Increase (%)",
       color = "Year") +
  #Add theme
  theme(
    plot.title = element_text(hjust = 0.5, family = "Arial", size = 18, face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1, family = "Arial", size = 12),  
    axis.text.y = element_text(size = 12, family = "Arial"),  
    legend.position = "top",  
    panel.grid.major = element_line(color = "gray90", size = 0.5), 
    panel.grid.minor = element_blank()  
  )
#Output the plot
ggsave("regression_plot_high_quality.pdf", width = 12, height = 8, device = cairo_pdf)

#Create years for comparison of dugout box price mechanisms over time
years <- c(2009, 2012)

#Initialize a for loop to loop through and regress both
models <- list()
for (yr in years) {
  data_subset <- all_red_sox_data %>%
    filter(sectiontype == "DUGOUTBOX", year == yr)
  
  model <- lm(logprice ~ dummy_1_5 + dummy_6_10 + dummy_11_15 + dummy_16_20 +
                dummy_21_30 + dummy_31_60 + dummy_61_90 + dummy_91_120 +
                dummy_121_150 + dummy_151_200, data = data_subset)
  
  #Store results into the models
  models[[paste("DUGOUTBOX", yr, sep = "_")]] <- model
}

#Output the results into .tex using Stargazer
stargazer(models, type = "latex", title = "Regression Results for Dugout Box (2009 & 2012)",
          dep.var.labels = "Log Price",
          column.labels = c("DUGOUTBOX 2009", "DUGOUTBOX 2012"),
          covariate.labels = c("1-5 Days", "6-10 Days", "11-15 Days", "16-20 Days",
                               "21-30 Days", "31-60 Days", "61-90 Days", "91-120 Days",
                               "121-150 Days", "151-200 Days"), 
          omit.stat = c("f", "ser"), digits = 3,
          #Output the latex file
          out = "dugoutbox_regression_results.tex")  








