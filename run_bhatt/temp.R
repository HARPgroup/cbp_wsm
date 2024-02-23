function (data, constants, ts = "daily", ...) 
{
    if (is.null(data$Tmax) | is.null(data$Tmin)) {
        stop("Required data missing for 'Tmax.daily' and 'Tmin.daily', or 'Temp.subdaily'")
    }
    Ta <- (data$Tmax + data$Tmin)/2
    P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
    delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 
        237.3)^2)
    gamma <- 0.00163 * P/constants$lambda
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
    delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
    w_s <- acos(-tan(constants$lat_rad) * tan(delta2))
    N <- 24/pi * w_s
    R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) * 
        sin(delta2) + cos(constants$lat_rad) * cos(delta2) * 
        sin(w_s))
    C_HS <- 0.00185 * (data$Tmax - data$Tmin)^2 - 0.0433 * (data$Tmax - 
        data$Tmin) + 0.4023
    ET_HS.Daily <- 0.0135 * C_HS * R_a/constants$lambda * (data$Tmax - 
        data$Tmin)^0.5 * (Ta + 17.8)
    ET.Daily <- ET_HS.Daily
    ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily, 
        "%m/%y"), FUN = sum)
    ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily, 
        "%m/%y"))), FUN = sum)
    ET.MonthlyAve <- ET.AnnualAve <- NULL
    for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
        i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 1
        ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon == 
            mon])
    }
    for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
        i = year - min(as.POSIXlt(data$Date.daily)$year) + 1
        ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year == 
            year])
    }
    ET_formulation <- "Hargreaves-Samani"
    ET_type <- "Reference Crop ET"
    message(ET_formulation, " ", ET_type)
    message("Evaporative surface: reference crop")
    results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly, 
        ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve, 
        ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation, 
        ET_type = ET_type)
    if (ts == "daily") {
        res_ts <- ET.Daily
    }
    else if (ts == "monthly") {
        res_ts <- ET.Monthly
    }
    else if (ts == "annual") {
        res_ts <- ET.Annual
    }
    message("Timestep: ", ts)
    message("Units: mm")
    message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
    if (NA %in% res_ts) {
        message(length(res_ts), " ET estimates obtained; ", length(which(is.na(res_ts))), 
            " NA output entries due to missing data")
        message("Basic stats (NA excluded)")
        message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
        message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
        message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
    }
    else {
        message(length(res_ts), " ET estimates obtained")
        message("Basic stats")
        message("Mean: ", round(mean(res_ts), digits = 2))
        message("Max: ", round(max(res_ts), digits = 2))
        message("Min: ", round(min(res_ts), digits = 2))
    }
    for (i in 1:length(results)) {
        namer <- names(results[i])
        write.table(as.character(namer), file = "ET_HargreavesSamani.csv", 
            dec = ".", quote = FALSE, col.names = FALSE, row.names = F, 
            append = TRUE, sep = ",")
        write.table(data.frame(get(namer, results)), file = "ET_HargreavesSamani.csv", 
            col.names = F, append = T, sep = ",")
    }
    invisible(results)
}

