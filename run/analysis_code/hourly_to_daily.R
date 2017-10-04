#This code translates the model data from hourly to data for comparison to the USGs data

uri_model_output <- 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSbD4ZuD68FK9ZqESd-_kWlsMhd1a66LWr1N8Xd5oo5zFZm3niigHhFiUJ98b2Y9ayQdncu-4q5VvL4/pub?output=csv'
model_output = read.csv(uri_model_output, header = TRUE, sep = ",", stringsAsFactors = FALSE);

daily_data <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(daily_data) <- c("year", "month", "day", "ovol")

z <- 1
x <- model_output[1,]
daily_data[z,1] <- x$year
daily_data[z,2] <- x$month
daily_data[z,3] <- x$day
daily_average <- sum(model_output$ovol[1:25]) * 0.504167
daily_data[z,4] <- daily_average

j <- 26
k <- 49
for (i in 2:8035){
  x <- model_output[j,]
  daily_data[i,1] <- x$year
  daily_data[i,2] <- x$month
  daily_data[i,3] <- x$day
  daily_average <- sum(model_output$ovol[j:k]) * 0.504167
  daily_data[i,4] <- daily_average
  j <- j+24
  k <- k+24
}

k <- k-1
x <- model_output[j,]
daily_data[8036,1] <- x$year
daily_data[8036,2] <- x$month
daily_data[8036,3] <- x$day
daily_average <- sum(model_output$ovol[j:k]) * 0.504167
daily_data[8036,4] <- daily_average

write.csv(daily_data, "C:\\Users\\alyssaf4\\Desktop\\R\\daily_model_data.csv")