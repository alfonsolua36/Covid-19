## Team Covid 1919
## Shirley Mach, Leah Skelton, Mayra Varillas, Alfonso Lua, and Sarah Truax
## shirleymach.12@gmail.com, leahskelton3@gmail.com, varillas.mayra1@gmail.com, alfonsolua36@gmail.com, sarahrtruax@gmail.com

## How we created our final data set to use with FIPS codes

    #data <- read.csv("CA_mobility_sentiment.csv")
    #data$date <- as.Date(data$date)
    #data$placeholder <- with(data,paste(sub_region_1, sub_region_2,sep=","))
    #write.csv(data,"DatawFIPS.csv")
    #datawFIPS <- read.csv("DatawFIPS.csv")
    #datawFIPS <- datawFIPS[,-17]
    #datawFIPS$polyname <- as.character(datawFIPS$polyname)
    #county.fips$polyname <- as.character(county.fips$polyname)
    #Data_FIPS <- merge(datawFIPS,county.fips,all.x = TRUE,by="polyname")
    #write.csv(Data_FIPS,"Data_FIPS.csv")

    # After creating this dataset we used Excel to make the polyname variable match that of county.fips
    # To recreate the maps below use the Data_FIPS.csv data set

## Set up for inital analysis

    Data_FIPS <- read.csv("Data_FIPS.csv")
    
    Data_FIPS$date <- as.Date(Data_FIPS$date,"%d/%m/%Y")
    
    par(mfrow=c(1,1))
    library(maps)
    library(ggplot2)
    library(dplyr)

## Map of First Date for Parks

    firstspeech <- Data_FIPS[Data_FIPS$date >= "2020-03-15" & Data_FIPS$date < "2020-03-22",]
    
    firstspeech <-  firstspeech[!is.na(firstspeech$parks_percent_change_from_baseline), ]
    firstspeech <-  firstspeech[!is.na(firstspeech$sub_region_2), ]
    
    firstspeech$polyname <- as.character(firstspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH1 <- merge(firstspeech,county.fips,all.x = TRUE)
    table(firstspeech$polyname)
    
    a <- SPEECH1 %>% group_by(fips) %>% summarise(mean=mean(parks_percent_change_from_baseline))
    summary(a)
    california.fips <- county.fips[c(seq(158,215)),]
    a <- left_join(california.fips,a,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    a$colorBuckets <- as.numeric(cut(a$mean,c(-50,-30,-20,-10,0,10,100)))
    leg.txt <- c("<-30", "-30,-20", "-20,-10", "-10,0", "0,10",">10")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- a$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("3/15 - 3/22",cex=1.5))
    mtext("Sentiment Score: 238",side=1,cex = 1.5)
    legend("topright", leg.txt, horiz = FALSE, fill = colors,cex = 1.5)

## Map of Second Date for Parks

    secondspeech <- Data_FIPS[Data_FIPS$date >= "2020-03-19" & Data_FIPS$date < "2020-03-26",]
    
    secondspeech <-  secondspeech[!is.na(secondspeech$parks_percent_change_from_baseline), ]
    secondspeech <-  secondspeech[!is.na(secondspeech$sub_region_2), ]
    
    secondspeech$polyname <- as.character(secondspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH2 <- merge(secondspeech,county.fips,all.x = TRUE)
    
    b <- SPEECH2 %>% group_by(fips) %>% summarise(mean=mean(parks_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    b <- left_join(california.fips,b,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    b$colorBuckets <- as.numeric(cut(b$mean,c(-50,-30,-20,-10,0,10,100)))
    leg.txt <- c("<-30", "-30,-20", "-20,-10", "-10,0", "0,10",">10")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- b$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("3/19 - 3/26",cex=1.5))
    mtext("Sentiment Score: 152",side=1,cex=1.5)

## Map of Third Date for Parks

    thirdspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-01" & Data_FIPS$date < "2020-04-08",]
    
    thirdspeech <-  thirdspeech[!is.na(thirdspeech$parks_percent_change_from_baseline), ]
    thirdspeech <-  thirdspeech[!is.na(thirdspeech$sub_region_2), ]
    
    thirdspeech$polyname <- as.character(thirdspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH3 <- merge(thirdspeech,county.fips,all.x = TRUE)
    table(thirdspeech$polyname)
    
    d <- SPEECH3 %>% group_by(fips) %>% summarise(mean=mean(parks_percent_change_from_baseline))
    View(county.fips)
    california.fips <- county.fips[c(seq(158,215)),]
    d <- left_join(california.fips,d,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    d$colorBuckets <- as.numeric(cut(d$mean,c(-50,-30,-20,-10,0,10,100)))
    leg.txt <- c("<-30", "-30,-20", "-20,-10", "-10,0", "0,10",">10")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- d$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/1 - 4/8",cex=1.5))
    mtext("Sentiment Score: 217",side=1,cex=1.5)

## Map of Fourth Date for Parks

    fourthspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-08" & Data_FIPS$date < "2020-04-15",]
    
    fourthspeech <-  fourthspeech[!is.na(fourthspeech$parks_percent_change_from_baseline), ]
    fourthspeech <-  fourthspeech[!is.na(fourthspeech$sub_region_2), ]
    
    fourthspeech$polyname <- as.character(fourthspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH4 <- merge(fourthspeech,county.fips,all.x = TRUE)
    table(fourthspeech$polyname)
    
    e <- SPEECH4 %>% group_by(fips) %>% summarise(mean=mean(parks_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    e <- left_join(california.fips,e,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    e$colorBuckets <- as.numeric(cut(e$mean,c(-50,-30,-20,-10,0,10,100)))
    leg.txt <- c("<-30", "-30,-20", "-20,-10", "-10,0", "0,10",">10")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- e$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/8 - 4/15",cex=1.5))
    mtext("Sentiment Score: 214",side=1,cex=1.5)

## Map of Fifth Date for Parks

    fifthspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-17" & Data_FIPS$date < "2020-04-24",]
    
    fifthspeech <-  fifthspeech[!is.na(fifthspeech$parks_percent_change_from_baseline), ]
    fifthspeech <-  fifthspeech[!is.na(fifthspeech$sub_region_2), ]
    
    fifthspeech$polyname <- as.character(fifthspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH5 <- merge(fifthspeech,county.fips,all.x = TRUE)
    table(secondspeech$polyname)
    
    f <- SPEECH5 %>% group_by(fips) %>% summarise(mean=mean(parks_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    f <- left_join(california.fips,f,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    f$colorBuckets <- as.numeric(cut(f$mean,c(-50,-30,-20,-10,0,10,100)))
    leg.txt <- c("<-30", "-30,-20", "-20,-10", "-10,0", "0,10",">10")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- f$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/17 - 4/24",cex=1.5))
    mtext("Sentiment Score: 294",side=1,cex=1.5)

## Analysis for Grocery stores

    summary(Data_FIPS$parks_percent_change_from_baseline)
    summary(Data_FIPS$grocery_and_pharmacy_percent_change_from_baseline)
    summary(Data_FIPS$retail_and_recreation_percent_change_from_baseline)
    summary(Data_FIPS$workplaces_percent_change_from_baseline)
    summary(Data_FIPS$residential_percent_change_from_baseline)

## Map of First Date for Grocery Stores

    firstspeech <- Data_FIPS[Data_FIPS$date >= "2020-03-15" & Data_FIPS$date < "2020-03-22",]
    
    firstspeech <-  firstspeech[!is.na(firstspeech$grocery_and_pharmacy_percent_change_from_baseline), ]
    firstspeech <-  firstspeech[!is.na(firstspeech$sub_region_2), ]
    
    firstspeech$polyname <- as.character(firstspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH1 <- merge(firstspeech,county.fips,all.x = TRUE)
    table(firstspeech$polyname)
    
    a <- SPEECH1 %>% group_by(fips) %>% summarise(mean=mean(grocery_and_pharmacy_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    a <- left_join(california.fips,a,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    a$colorBuckets <- as.numeric(cut(a$mean,c(-50,-15,-10,-5,0,5,100)))
    leg.txt <- c("<-15", "-15,-10", "-10,-5", "-5,0", "0,5",">5")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- a$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("3/15 - 3/22",cex=1.5))
    mtext("Sentiment Score: 238",side=1,cex=1.5)
    legend("topright", leg.txt, horiz = FALSE, fill = colors,cex = 1.5)

## Map of Second Date for Grocery Stores

    secondspeech <- Data_FIPS[Data_FIPS$date >= "2020-03-19" & Data_FIPS$date < "2020-03-26",]
    
    secondspeech <-  secondspeech[!is.na(secondspeech$grocery_and_pharmacy_percent_change_from_baseline), ]
    secondspeech <-  secondspeech[!is.na(secondspeech$sub_region_2), ]
    
    secondspeech$polyname <- as.character(secondspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH2 <- merge(secondspeech,county.fips,all.x = TRUE)
    
    b <- SPEECH2 %>% group_by(fips) %>% summarise(mean=mean(grocery_and_pharmacy_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    b <- left_join(california.fips,b,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    b$colorBuckets <- as.numeric(cut(b$mean,c(-50,-15,-10,-5,0,5,100)))
    leg.txt <- c("<-15", "-15,-10", "-10,-5", "-5,0", "0,5",">5")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- b$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("3/19 - 3/26",cex=1.5))
    mtext("Sentiment Score: 152",side=1,cex=1.5)

## Map of Third Date for Grocery Stores

    thirdspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-01" & Data_FIPS$date < "2020-04-08",]
    
    thirdspeech <-  thirdspeech[!is.na(thirdspeech$grocery_and_pharmacy_percent_change_from_baseline), ]
    thirdspeech <-  thirdspeech[!is.na(thirdspeech$sub_region_2), ]
    
    thirdspeech$polyname <- as.character(thirdspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH3 <- merge(thirdspeech,county.fips,all.x = TRUE)
    table(thirdspeech$polyname)
    
    d <- SPEECH3 %>% group_by(fips) %>% summarise(mean=mean(grocery_and_pharmacy_percent_change_from_baseline))
    View(county.fips)
    california.fips <- county.fips[c(seq(158,215)),]
    d <- left_join(california.fips,d,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    d$colorBuckets <- as.numeric(cut(d$mean,c(-50,-15,-10,-5,0,5,100)))
    leg.txt <- c("<-15", "-15,-10", "-10,-5", "-5,0", "0,5",">5")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- d$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/1 - 4/8",cex=1.5))
    mtext("Sentiment Score: 217",side=1,cex=1.5)
    
## Map of Fourth Date for Grocery Stores

    fourthspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-08" & Data_FIPS$date < "2020-04-15",]
    
    fourthspeech <-  fourthspeech[!is.na(fourthspeech$grocery_and_pharmacy_percent_change_from_baseline), ]
    fourthspeech <-  fourthspeech[!is.na(fourthspeech$sub_region_2), ]
    
    fourthspeech$polyname <- as.character(fourthspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH4 <- merge(fourthspeech,county.fips,all.x = TRUE)
    table(fourthspeech$polyname)
    
    e <- SPEECH4 %>% group_by(fips) %>% summarise(mean=mean(grocery_and_pharmacy_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    e <- left_join(california.fips,e,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    e$colorBuckets <- as.numeric(cut(e$mean,c(-50,-15,-10,-5,0,5,100)))
    leg.txt <- c("<-15", "-15,-10", "-10,-5", "-5,0", "0,5",">5")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- e$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/8 - 4/15",cex=1.5))
    mtext("Sentiment Score: 214",side=1,cex=1.5)

## Map of Fifth Date for Grocery Stores

    fifthspeech <- Data_FIPS[Data_FIPS$date >= "2020-04-17" & Data_FIPS$date < "2020-04-24",]
    
    fifthspeech <-  fifthspeech[!is.na(fifthspeech$grocery_and_pharmacy_percent_change_from_baseline), ]
    fifthspeech <-  fifthspeech[!is.na(fifthspeech$sub_region_2), ]
    
    fifthspeech$polyname <- as.character(fifthspeech$polyname)
    county.fips$polyname <- as.character(county.fips$polyname)
    
    SPEECH5 <- merge(fifthspeech,county.fips,all.x = TRUE)
    table(secondspeech$polyname)
    
    f <- SPEECH5 %>% group_by(fips) %>% summarise(mean=mean(grocery_and_pharmacy_percent_change_from_baseline))
    california.fips <- county.fips[c(seq(158,215)),]
    f <- left_join(california.fips,f,by="fips")
    
    colors = c("#006600", "#009900", "#33FF33", "#FFFF33", "#FF0000","#990000")
    f$colorBuckets <- as.numeric(cut(f$mean,c(-50,-15,-10,-5,0,5,100)))
    leg.txt <- c("<-15", "-15,-10", "-10,-5", "-5,0", "0,5",">5")
    cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
    colorsmatched <- f$colorBuckets
    map("county", "california",col=colors[colorsmatched],fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
    map("county","california", col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
        projection="polyconic")
    title(main=list("4/17 - 4/24",cex=1.5))
    mtext("Sentiment Score: 294",side=1,cex=1.5)