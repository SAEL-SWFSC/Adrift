# Report Plot Creation Functions

# TODO: Use PAMpal:::mapWavFolder to create "effort" column describing
#       how many minutes of each hour we were actually recording.
# TODO: Check Raven input on someones actual tables
# 2023-11-02: First version, basic binned hourly presence and data loading
# 2023-12-04: Adjusting to account for possible "End.time" or "end" columns instead
#             of only assuming detection at start time
# 2024-01-03: Adding better warning messages to catch time errors
# 2024-01-26: Effort wasnt proper for no-detection drifts
# 2024-01-28: Migrating to ADRIFT_Report repo and adding functions from PAMscapes

if(packageVersion('ggplot2') < '3.5.0') {
    install.packages('ggplot2')
}
if(packageVersion('PAMmisc') < '1.11.9') {
    devtools::install_github('TaikiSan21/PAMmisc')
}
if(packageVersion('PAMscapes') < '0.5.7') {
    devtools::install_github('TaikiSan21/PAMscapes')
}

library(lubridate)
# I had to also install and load the Rcpp package to the raster and marmap package to load (KB 4/16)
#install.packages("Rcpp") 
library(Rcpp)
library(raster)
library(dplyr)
library(PAMmisc)
library(PAMpal)
library(readxl)
library(ggplot2)
library(RSQLite)
library(patchwork)
library(PAMscapes)
library(purrr)
library(stringr)
library(marmap)
library(sf)
library(RColorBrewer)
library(ggbreak)

getDbEffort <- function(db, bin='hour') {
    con <- dbConnect(db, drv=SQLite())
    on.exit(dbDisconnect(con))
    depDet <- dbReadTable(con, 'deploymentData')
    depDet$DataStart <- as.POSIXct(depDet$DataStart, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    depDet$DataEnd <- as.POSIXct(depDet$DataEnd, format='%Y-%m-%d %H:%M:%S', tz='UTC')
    hasNa <- is.na(depDet$DataStart) | is.na(depDet$DataEnd)
    depDet <- depDet[!hasNa, ]
    
    bind_rows(lapply(split(depDet, depDet$DriftName), function(x) {
        thisEffort <- timesToBins(c(x$DataStart, x$DataEnd), bin=bin)
        thisEffort$DeploymentSite <- x$DeploymentSite[1]
        thisEffort$DriftName <- x$DriftName[1]
        thisEffort
    }))
}

effortToBins <- function(effort, bin='hour', off=NULL) {
    # if(bin == 'hour') {
    #   mins <- effortToBins(effort, bin='min')
    #   mins$hour <- floor_date(mins$UTC, unit='1hour')
    #   return(mins %>% 
    #     group_by(hour, DriftName, DeploymentSite) %>% 
    #     summarise(pctEff = n() / 60) %>% 
    #     ungroup() %>% 
    #     rename(UTC = hour)
    #   )
    # }
    denom <- switch(bin,
                    'hour' = 60,
                    'day' = 1440,
                    1
    )
    off <- formatOffs(off)
    
    bind_rows(lapply(split(effort, effort$DriftName), function(x) {
        mins <- timesToBins(c(x$start, x$end), bin='min')
        mins$BIN <- floor_date(mins$UTC, unit=bin)
        if(!is.null(off) &&
           x$DriftName[1] %in% off$DriftName) {
            thisOff <- filter(off, DriftName == x$DriftName[1])
            mins <- mins[!mins$BIN %in% thisOff$UTC, ]
        }
        
        thisEffort <- mins %>% 
            group_by(BIN) %>% 
            summarise(pctEff = n() / denom) %>% 
            ungroup() %>% 
            rename(UTC = BIN)
        thisEffort$DeploymentSite <- x$DeploymentSite[1]
        thisEffort$DriftName <- x$DriftName[1]
        thisEffort
    }))
}

formatOffs <- function(x) {
    if(is.null(x)) {
        return(x)
    }
    if('UTC' %in% colnames(x) &&
       !'start' %in% colnames(x)) {
        x$start <- x$UTC
    }
    bind_rows(lapply(1:nrow(x), function(i) {
        thisOff <- timesToBins(c(x$start[i], x$end[i]), bin='min')
        thisOff$DriftName <- x$DriftName[i]
        thisOff
    }))
}

timesToBins <- function(x, bin='hour') {
    thisRange <- c(floor_date(min(x, na.rm=TRUE), unit=bin),
                   ceiling_date(max(x, na.rm=TRUE), unit=bin))
    dateSeq <- seq(from=thisRange[1], to=thisRange[2], by=bin)
    data.frame(UTC = dateSeq)
}

gpsToEffort <- function(gps, bin='hour') {
    gps <- filter(gps, recordingEffort)
    bind_rows(lapply(split(gps, gps$DriftName), function(x) {
        thisEffort <- timesToBins(x$UTC)
        thisEffort$DriftName <- x$DriftName[1]
        thisEffort$DeploymentSite <- x$DeploymentSite[1]
        thisEffort
    }))
}

# formats data into time bins based on "bin".
# Output has UTC, Lat/Long, species, call
# Last two columns are NA if no presence
formatBinnedPresence <- function(x, 
                                 effort=NULL,
                                 bin='hour', 
                                 gps,
                                 joinBy='DriftName',
                                 format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                          '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'), 
                                 tz='UTC',
                                 gpsThresh=3600*3) {
    
    # for future DriftName could be given as a "by" column if need to extend
    gps <- checkGps(gps, format=format, tz=tz)
    if(is.null(effort)) {
        effort <- gpsToEffort(gps, bin=bin)
    }
    x$UTC <- floor_date(x$UTC, unit=bin)
    if('DriftName' %in% colnames(x)) {
        x$DriftName <- toupper(x$DriftName)
    }
    if('end' %in% colnames(x) &&
       any(!is.na(x$end))) {
        x$end <- floor_date(x$end, unit=bin)
        x <- bind_rows(lapply(1:nrow(x), function(i) {
            if(is.na(x$end[i])) {
                return(x[i, ])
            }
            dates <- seq(from=x$UTC[i], to=x$end[i], by=bin)
            result <- as.list(x[i, ])
            result$UTC <- dates
            result
        }))
        x$end <- NULL
    }
    x <- distinct(x[c('UTC', 'species', 'call', 'DriftName')])
    
    # result <- vector('list', length=length(unique(x$DriftName)))
    # names(result) <- unique(x$DriftName)
    result <- vector('list', length=length(unique(effort[[joinBy]])))
    names(result) <- unique(effort[[joinBy]])
    for(i in seq_along(result)) {
        if(!joinBy %in% colnames(gps)) {
            thisGps <- gps
        } else {
            thisGps <- gps[gps[[joinBy]] == names(result)[i], ]
        }
        noGps <- is.null(thisGps) || nrow(thisGps) == 0
        
        thisResult <- effort[effort[[joinBy]] == names(result)[i], ]
        thisData <- x[x[[joinBy]] == names(result)[i], ]
        thisData[[joinBy]] <- NULL
        # dateSeq <- seq(from=thisRange[1], to=thisRange[2], by=bin)
        # thisResult <- data.frame(UTC = dateSeq) #, DriftName=names(result)[i])
        thisResult <- left_join(thisResult, thisData,
                                # thisResult <- left_join(thisResult, x,
                                # c('UTC', 'species', 'call')],
                                by='UTC')
        if(noGps) {
            warning('Could not find GPS for drift ', names(result)[i])
        } else {
            thisResult <- PAMpal::addGps(thisResult, thisGps, thresh=gpsThresh)
            if('DeploymentSite' %in% colnames(thisGps)) {
                thisResult$DeploymentSite <- thisGps$DeploymentSite[1]
            }
        }
        result[[i]] <- thisResult
    }
    #now utcs, lat/long, species, call at floor_date
    result <- distinct(bind_rows(result))
    years <- unique(year(result$UTC))
    result$year <- factor(year(result$UTC), levels=min(years):max(years))
    result
}

# Loads and formats detection data for use in above.
# Output has columns UTC, species, call, and DriftName
# call can be all NA if calltype is not logged
loadDetectionData <- function(x, 
                              source=c('csv', 'triton', 'df', 'raven', 'bm'), 
                              driftName=NULL,
                              driftPattern='([A-z]*_[0-9]{1,3})_.*',
                              format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                       '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'),
                              speciesCol='species',
                              typeCol=NULL,
                              tz='UTC',
                              sheet=c('Detections', 'AdhocDetections')) {
    if(length(x) > 1) {
        return(bind_rows(lapply(x, function(file) {
            loadDetectionData(file, source=source, driftName=driftName,
                              driftPattern=driftPattern, format=format,
                              speciesCol=speciesCol, typeCol=typeCol,
                              tz=tz, sheet=sheet)
        })))
    }
    switch(match.arg(source),
           'csv' = {
               if(is.null(driftName)) {
                   driftName <- gsub(driftPattern, '\\1', basename(x))
                   if(driftName == basename(x)) {
                       warning('Drift pattern could not parse file ', basename(x),
                               ', fix pattern or provide name directly to "driftName"')
                       return(NULL)
                   }
               }
               x <- read.csv(x, stringsAsFactors = FALSE)
               x$DriftName <- driftName
               return(loadDetectionData(x, source='df', driftName=NULL, format=format,
                                        speciesCol=speciesCol, typeCol=typeCol, tz=tz))
           },
           'triton' = {
               x <- loadTritonLog(x, driftPattern=driftPattern, driftName=driftName, tz=tz, sheet=sheet)
           },
           'df' = {
               if(!'species' %in% colnames(x)) {
                   if(!speciesCol %in% colnames(x)) {
                       warning('Must provide correct species ID column to "speciesCol"')
                       return(NULL)
                   }
                   x$species <- x[[speciesCol]]
                   x[[speciesCol]] <- NULL
               }
               if(!'call' %in% colnames(x)) {
                   if(is.null(typeCol) ||
                      !typeCol %in% colnames(x)) {
                       x$call <- NA
                   } else if(typeCol %in% colnames(x)) {
                       x$call <- x[[typeCol]]
                       x[[typeCol]] <- NULL
                   }
               }
               if(!'DriftName' %in% colnames(x)) {
                   if(is.null(driftName)) {
                       warning('Must provide "driftName" if no "DriftName" column present.')
                       return(NULL)
                   }
                   x$DriftName <- driftName
               }
               if(!'UTC' %in% colnames(x)) {
                   warning('Must have column "UTC"')
                   return(NULL)
               }
               x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
               if('duration' %in% colnames(x)) {
                   x$end <- x$UTC + x$end
               }
               if('end' %in% colnames(x)) {
                   x$end <- parseToUTC(x$end, format=format, tz=tz)
               }
           },
           'bm' = {
               if(grepl('xls$', x)) {
                   x <- read_xls(x)
               } else if(grepl('xlsx$', x)) {
                   x <- read_xlsx(x)
               }
               x <- rename(x,
                           'UTC' = 'Start_time',
                           'species' = 'Species_Code',
                           'call' = 'Call')
               x$end <- NA
               x$DriftName <- paste0(driftName, '_', formatNumber(x$Drift))
               x <- x[c('UTC', 'species', 'call', 'end', 'DriftName')]
           },
           'raven' = {
               #PAMmisc::formatAnno has fmtRaven
               # creates UTC, Duration, f1, f2, Label from
               # BeginTimes, DeltaTimes, LowFrq, HighFreq, Annotation
               x <- PAMmisc:::fmtRaven(x)
               x <- rename(x, species=Label)
               x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
               x$end <- x$UTC + x$Duration
               x$call <- NA
               # MISSING:::: DriftName stuff. Unsure logic of raven file names
               x$DriftName <- driftName
           }
    )
    if(is.null(x) ||
       nrow(x) == 0) {
        return(x)
    }
    naStarts <- is.na(x$UTC)
    if(any(naStarts)) {
        warning(sum(naStarts), ' times were not able to be processed in drift(s): ',
                paste0(unique(x$DriftName[naStarts]), collapse=', '))
    }
    naBounds <- is.na(x$end) | naStarts
    if(!all(naBounds)) {
        endBefore <- x$end[!naBounds] < x$UTC[!naBounds]
        if(any(endBefore)) {
            warning(sum(endBefore), ' end times were before start times in drift(s): ',
                    paste0(unique(x$DriftName[endBefore]), collapse=', '))
        }
    }
    x
}

formatNumber <- function(x) {
    outs <- as.character(x)
    outs[x < 10] <- paste0('0', outs[x < 10])
    outs[x < 100] <- paste0('0', outs[x < 100])
    outs
}

checkGps <- function(x,
                     format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                              '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'),
                     tz='UTC') {
    if(is.character(x)) {
        if(!file.exists(x)) {
            warning('File ', x, ' does not exist')
            return(NULL)
        }
        x <- read.csv(x, stringsAsFactors = FALSE)
    }
    needCols <- c('UTC', 'Latitude', 'Longitude', 'DriftName')
    missCols <- !needCols %in% colnames(x)
    if(any(missCols)) {
        warning('GPS must have column(s) ', paste0(needCols[missCols], collapse=', '))
        return(NULL)
    }
    x$UTC <- parseToUTC(x$UTC, format=format, tz=tz)
    x
}

parseToUTC <- function(x, 
                       format=c('%m/%d/%Y %H:%M:%S', '%m-%d-%Y %H:%M:%S',
                                '%Y/%m/%d %H:%M:%S', '%Y-%m-%d %H:%M:%S'), 
                       tz,
                       excel=FALSE) {
    tryCatch({
        testTz <- parse_date_time('10-10-2020 12:00:05', orders = '%m/%d/%Y %H:%M:%S', tz=tz)
    },
    error = function(e) {
        msg <- e$message
        if(grepl('CCTZ: Unrecognized output timezone', msg)) {
            stop('Timezone not recognized, see function OlsonNames() for accepted options', call.=FALSE)
        }
    })
    if(all(is.na(x))) {
        return(x)
    }
    if(is.numeric(x) && isTRUE(excel)) {
        x <- as.POSIXct(x * 24 * 3600, origin = '1899-12-30', tz=tz)
    }
    if(!inherits(x, 'POSIXct')) {
        origTz <- parse_date_time(x, orders=format, tz=tz, exact=TRUE, truncated=3, quiet=TRUE)
        if(!inherits(origTz, 'POSIXct')) {
            stop('Unable to convert to POSIXct time.', call.=FALSE)
        }
    } else {
        origTz <- x
    }
    with_tz(origTz, tzone='UTC')
}

loadTritonLog <- function(x, 
                          driftPattern='([A-z]*_[0-9]{1,3})_.*',
                          driftName=NULL,
                          tz='UTC',
                          sheet=c('Detections', 'AdhocDetections')) {
    if(length(x) > 1) {
        return(
            bind_rows(lapply(x, function(f) {
                loadTritonLog(f, driftPattern, driftName, tz,
                              sheet)
            }))
        )
    }
    
    if(length(sheet) > 1) {
        return(bind_rows(lapply(sheet, function(s) {
            loadTritonLog(x, driftPattern, driftName, tz,
                          sheet=s)
        })))
    }
    if(is.null(driftName)) {
        driftName <- gsub(driftPattern, '\\1', basename(x))
        if(driftName == basename(x)) {
            warning('Drift pattern could not parse file ', basename(x),
                    ', fix pattern or provide name directly to "driftName"')
            return(NULL)
        }
    }
    isExcel <- FALSE
    if(grepl('csv$', x)) {
        x <- read.csv(x, stringsAsFactors = FALSE)
    } else if(grepl('xls$', x)) {
        isExcel <- TRUE
        x <- read_xls(x, sheet=sheet)
    } else if(grepl('xlsx$', x)) {
        isExcel <- TRUE
        x <- read_xlsx(x, sheet=sheet)
    }
    if(is.character(x) || is.null(x) || nrow(x) == 0) {
        return(NULL)
    }
    # isExcel <- FALSE
    nameDf <- data.frame(
        old = c('species.code', 'species code','start time', 'start.time', 'end time', 'end.time'),
        new = c('species', 'species', 'utc', 'utc', 'end', 'end')
    )
    colnames(x) <- tolower(colnames(x))
    for(i in 1:nrow(nameDf)) {
        hasThis <- colnames(x) == nameDf$old[i]
        if(!any(hasThis)) {
            next
        }
        colnames(x)[hasThis] <- nameDf$new[i]
    }
    # x <- x[c('Input.file', 'Event.Number', 'Species.Code', 'Call', 'UTC')]
    tritonCols <- c('utc', 'species', 'call', 'end')
    if(!all(tritonCols %in% colnames(x))) {
        warning('Not all expected columns found in file ', x,
                ' are you sure this is Triton output?')
        return(NULL)
    }
    
    
    x$DriftName <- driftName
    
    x <- x[c(tritonCols, 'DriftName')]
    colnames(x)[1] <- 'UTC'
    x$UTC <- parseToUTC(x$UTC, tz=tz, excel=isExcel)
    x$end <- parseToUTC(x$end, tz=tz, excel=isExcel)
    x
}

shortenOffs <- function(x, nMax=7) {
    result <- bind_rows(
        lapply(
            split(x, x$offGroup), function(g) {
                if(all(!g$off)) {
                    return(g)
                }
                isLast <- g$offGroup[1] == max(x$offGroup)
                if(isLast) {
                    # lastVal <- max(g$binDate) - 24 * 3600
                }
                ons <- g[!g$off, ]
                offs <- g[g$off, ]
                last <- min(nrow(offs), nMax)
                offs <- offs[c(1:last), ]
                if(isLast) {
                    # offs$binDate[nrow(offs)] <- lastVal
                }
                rbind(ons, offs)
            }
        )
    )
    years <- unique(year(result$binDate))
    result$months <- month(result$binDate)
    # if(length(years) == 1 &&
    #    any(result$months == 12)) {
    #     year(result$binDate[result$months == 12]) <- years - 1
    # }
    result <- arrange(result, binDate)
    result$plotX <- 1:nrow(result)
    # if(length(years) == 1 &&
    #    any(result$months == 12)) {
    #     year(result$binDate[result$months == 12]) <- years
    # }
    result$months <- NULL
    result
}

# labels which time bins have effort and how much
# "by" is effort column
markNumEffort <- function(x, 
                          by='DriftName',
                          bin='hour/day', 
                          keepCols=c('species', 'call')) {
    bin <- strsplit(bin, '/')[[1]]
    bin <- gsub('s$', '', bin)
    if(length(bin) == 1) {
        bin <- c(bin, bin)
    }
    x$UTC <- floor_date(x$UTC, unit=bin[1])
    x$binDate <- floor_date(x$UTC, unit=bin[2])
    # dateSeq <- seq(from=min(x$binDate), to=max(x$binDate), by=bin)
    dateSeq <- seq(from=floor_date(min(x$binDate), unit='year'),
                   to = ceiling_date(max(x$binDate), unit='year')-period(1, 'day'),
                   by=bin[2])
    if(!'pctEff' %in% colnames(x)) {
        x$pctEff <- 1
    }
    effort <- x %>%
        select(all_of(c('UTC', 'binDate', 'pctEff', by))) %>%
        distinct() %>%
        group_by(binDate) %>%
        summarise(nEffort=sum(pctEff)) %>%
        ungroup()
    missDates <- !dateSeq %in% effort$binDate
    effort <- bind_rows(effort, data.frame(binDate=dateSeq[missDates], nEffort=0))
    effort <- arrange(effort, binDate)
    effort$group <- FALSE
    effort$off <- effort$nEffort == 0
    effort$offGroup <- FALSE
    effort$offGroup[1] <- effort$off[1]
    effort$group[1] <- TRUE
    for(i in 2:nrow(effort)) {
        effort$group[i] <- effort$nEffort[i-1] == 0 & effort$nEffort[i] != 0
        effort$offGroup[i] <- isFALSE(effort$off[i-1]) & isTRUE(effort$off[i])
    }
    effort$offGroup <- cumsum(effort$offGroup)
    effort$nGroup <- cumsum(effort$group)
    effort$season <- markSeason(effort$binDate)
    
    # effort$year <- year(effort$binDate)
    x <- distinct(select(x, any_of(c('UTC', 'binDate','year',  by, keepCols))))
    x$season <- markSeason(x$binDate)
    if(!'year' %in% colnames(x)) {
        years <- unique(year(effort$binDate))
        x$year <- factor(year(x$binDate), levels=min(years):max(years))
    }
    effort$year <- factor(year(effort$binDate), levels=levels(x$year))
    # x$year <- year(x$binDate)
    list(dates=dateSeq, data=x, effort=effort)
}

# adds points to square corners for line plot
# loc is where to put the new lines
formatEffortPlot <- function(x, loc=.5, buffer=.001) {
    higher <-which(c(FALSE,  x$nEffort[2:nrow(x)] > x$nEffort[1:(nrow(x)-1)]))
    lower <- which(c(FALSE, x$nEffort[2:nrow(x)] < x$nEffort[1:(nrow(x)-1)]))
    highDf <- x[higher, ]
    highDf$nEffort <- x$nEffort[higher]
    highDf$plotX <- highDf$plotX - loc
    highDf <- rbind(highDf, highDf)
    highDf$nEffort[1:length(higher)] <- x$nEffort[higher-1]
    highDf$plotX[1:length(higher)] <- highDf$plotX[1:length(higher)] - buffer
    
    lowDf <- x[lower, ]
    lowDf$nEffort <- x$nEffort[lower-1]
    lowDf$plotX <- lowDf$plotX - loc
    lowDf <- rbind(lowDf, lowDf)
    lowDf$nEffort[1:length(lower)] <- x$nEffort[lower]
    lowDf$plotX[1:length(lower)] <- lowDf$plotX[1:length(lower)] + buffer
    rbind(x, lowDf, highDf) %>%
        arrange(plotX)
}

# x - data from formatBinnedPresence
# maxEff - can be left NULL, used to enforce different y-axis limit
# legend - show, blank, or remove the legend from final plot
# botAxis - TRUE or FALSE to show labels on bottom axis
# by - column in data to split plots by, typically 'region' or NULL
# leftLab - labels for left side of plot, usually used automatically
#           with "by" so can be left NULL
# title - title for plot
# plotEffort - TRUE/FALSE to show lower effort subplots
# barPosition - 'stack' for default geom_bar, 'dodge' for side by side
# combineYears - TRUE/FALSE to combine all years together

plotYearlyPresence <- function(x, 
                               percent=TRUE, 
                               maxEff=NULL,
                               legend=c('show', 'blank', 'remove'),
                               botAxis=TRUE, 
                               by=NULL, 
                               leftLab=NULL,
                               title=NULL,
                               plotEffort=TRUE,
                               barPosition='stack',
                               combineYears=FALSE,
                               shadeSeasons=TRUE) {
    if(!is.null(by) && by == 'region' &&
       !by %in% colnames(x)) {
        x <- markPCRegion(x)
    }
    
    if(isTRUE(combineYears)) {
        year(x$UTC) <- year(x$UTC[1])
        x$year <- NULL
        legend <- 'remove'
    }
    if(!is.null(by) && by %in% colnames(x)) {
        splitData <- split(x, x[[by]])
        legendIx <- floor(median(seq_along(splitData)))
        result <- vector('list', length=length(splitData))
        for(i in seq_along(result)) {
            if(nrow(splitData[[i]]) == 0) next
            result[[i]] <- plotYearlyPresence(splitData[[i]],
                                              percent=percent,
                                              maxEff=maxEff,
                                              # legend=ifelse(i==legendIx, 'show', 'blank'),
                                              legend=legend,
                                              botAxis=i==length(result),
                                              by=NULL,
                                              leftLab = names(splitData)[i],
                                              title=NULL,
                                              plotEffort=plotEffort,
                                              barPosition=barPosition,
                                              combineYears=combineYears,
                                              shadeSeasons=shadeSeasons)
        }
        result <- result[!sapply(result, is.null)]
        out <- wrap_plots(result) + plot_layout(ncol=1, guides = 'collect')
        if(!is.null(title)) {
            out <- out +
                plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=.5)))
        }
        return(out)
    }
    if(is.data.frame(x)) {
        x <- markNumEffort(x, keepCols=c('species', 'call'))
    }
    effort <- bind_rows(lapply(split(x$effort, x$effort$year), function(y) {
        if(nrow(y) <= 1) {
            # print(z)
            return(NULL)
        }
        shortenOffs(y, nMax=364)
    })
    )
    
    data <- x$data
    # browser()
    data <- left_join(data, effort[c('plotX', 'binDate', 'nEffort')],
                      by=join_by(binDate))
    labs <- list(ix = seq(from=min(effort$plotX), to=max(effort$plotX), length.out=5),
                 label = seq(from=min(effort$binDate), to=min(effort$binDate) + period(364, units='days'), length.out=5))
    
    labs$label <- format(labs$label, '%b-%d')
    effort <- formatEffortPlot(effort)
    if(is.null(maxEff)) {
        maxEff <- max(effort$nEffort)
    }
    effPlot <- ggplot(effort) +
        geom_path(data=effort, aes(x=plotX, y=nEffort, col=year, group=year)) +
        scale_color_manual(values=scales::hue_pal()(length(levels(data$year))),
                           limits=levels(data$year)) 
        theme_bw()  
        
    # scale_y_continuous(breaks=(1:10)*24, name='Hours', limits=c(0, maxEff))
    data <- filter(data, !is.na(species))
    if(percent) {
        # browser()
        # THIS IST WORKING WHY. Mean effort is way higher than n()
        data <- data %>%
            group_by(plotX, year) %>%
            summarise(n=n(), pct=n()/mean(nEffort), eff=mean(nEffort), .groups='drop_last') %>%
            ungroup()
        binPlot <- ggplot() +
            geom_rect(data=data, aes(xmin=plotX-.45,xmax=plotX+.45, ymin=0, ymax=pct, fill=year))
        # geom_line(data=formatEffortPlot(effort), aes(x=plotX, y=nEffort / ymax *24, alpha=TRUE)) +
        # scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, 1),
        #                    # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
        #                    breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
        # adding effort > 0 lines to base plot
        binPlot <- binPlot +
            geom_path(data=effort, aes(x=plotX, y=as.numeric(nEffort > 0), group=year), col='black') 
    } else {
        # making separate scale for each year's max effort
        blankData <- effort %>%
            group_by(year) %>%
            summarise(plotX=min(plotX), max=max(nEffort))
        binPlot <- ggplot() +
            geom_bar(data=data, aes(fill=year, x=plotX), position=barPosition, width=1) +
            geom_blank(data=blankData, aes(x=plotX, y=max))
        # adding effort lines to base plot
        binPlot <- binPlot +
            geom_path(data=effort, aes(x=plotX, y=nEffort, group=year), col='black') 
        
    }
    
    if(shadeSeasons) {
        shadeAlpha <- 0.15
        seasonDf <- data.frame(xmin=c(-18, 61, 183, 336),
                               xmax=c(61, 183, 336, 365+19),
                               ymin=c(0, 0, 0, 0),
                               ymax=rep(maxEff*1.05, 4),
                               season=c('Winter', 'Upwelling', 'Post-Upwelling', 'Winter'))
        binPlot <- binPlot +
            geom_rect(data=filter(seasonDf, season == 'Winter'), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                      fill=seasonColors['Winter'], alpha=shadeAlpha) +
            geom_rect(data=filter(seasonDf, season == 'Upwelling'), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                      fill=seasonColors['Upwelling'], alpha=shadeAlpha) +
            geom_rect(data=filter(seasonDf, season == 'Post-Upwelling'), 
                      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                      fill=seasonColors['Post-Upwelling'], alpha=shadeAlpha)
            
    }
    binPlot <- binPlot + theme_bw()
    if(barPosition == 'stack') {
        binPlot <- binPlot +
            facet_wrap(~year, ncol=1, drop=FALSE, scales='free_y')
    }
    if(combineYears) {
        fillColors <- scales::hue_pal()(length(levels(data$year)))
        fillColors <- 'firebrick1'
    } else {
        fillColors <- scales::hue_pal()(length(levels(data$year)))
    }
    fillLims <- levels(data$year)
    binPlot <- binPlot +
        scale_fill_manual(values=fillColors, limits=fillLims)
    if(isFALSE(botAxis)) {
        # case when breaks=labs$ix, labels=NULL
        effLabs <- NULL
        binLabs <- NULL
    } else {
        # case when labels=labs$label
        effLabs <- labs$label
        if(isTRUE(plotEffort)) {
            # case when labels=NULL
            binLabs <- NULL
        } else {
            binLabs <- labs$label
        }
    }
    effPlot <- effPlot +
        scale_x_continuous(breaks=labs$ix, labels=effLabs, name=NULL)
    binPlot <- binPlot + 
        scale_x_continuous(breaks=labs$ix, labels=binLabs, name=NULL)
    
    
    switch(match.arg(legend),
           'remove' = {
               binPlot <- binPlot + theme(legend.position='none')
               effPlot <- effPlot + theme(legend.position='none')
           },
           'blank' = {
               binPlot <- binPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)),
                          fill = guide_legend(override.aes = list(fill=NA)))
               effPlot <- effPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)))
           },
           'show' = {
               effPlot <- effPlot +
                   theme(legend.key = element_rect(fill = "white"),
                         legend.text = element_text(color = "white"),
                         legend.title = element_text(color = "white")) +
                   guides(color = guide_legend(override.aes = list(color = NA)))
           }
    )

    effPlot <- effPlot +
        scale_y_continuous(breaks=(1:15)*24, name='Hours', limits=c(0, maxEff),
                           expand=expansion(mult=c(0, .05)))
    if(isTRUE(percent)) {
        binYLim <- c(0, 1)
        binPlot <- binPlot +
            scale_y_continuous(expand=expansion(mult=c(0, 0.05)), 
                               # sec.axis=sec_axis(trans = ~.*ymax/24, breaks=seq(from=0, to=ymax, by=24)),
                               breaks=c(0,.25, .5, .75, 1), name='Percent of Avail. Hours')
    } else {
        binYLim <- c(0, maxEff)
        binPlot <- binPlot +
            scale_y_continuous(expand=expansion(mult=c(0, .05)) , #limits=c(0, maxEff),
                               breaks=seq(from=0, to=max(effort$nEffort), by=24), name='Hours') 
    }
    # }
    binPlot <- binPlot +
        coord_cartesian(ylim=binYLim, xlim=c(1, max(effort$plotX))) +
        theme(
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text = element_text(size = 14),#Shannon added at request of BOEM editors
            axis.title = element_text(size = 15)
            
        )
    if(plotEffort) {
        out <- binPlot/effPlot + plot_layout(heights=c(5,1), ncol=1)
    } else {
        out <- binPlot
    }
    if(!is.null(leftLab)) {
        out <- wrap_elements(grid::textGrob(leftLab, rot=90)) + out +
            plot_layout(widths=c(1,40)) 
    }
    if(!is.null(title)) {
        out <- out +
            plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=.5)))
    }
    out
}

plotRadialPresence <- function(x, bin=c('hour', 'month'), title=NULL) {
    bin <- match.arg(bin)
    switch(bin, 
           'hour' = {
               x$PLOTBIN <- (hour(x$UTC) - 7) %% 24
               lims <- c(-.5, 23.5)
               breaks <- seq(from=0, to=24, by=3)
               pStart <- -(.5/24)*2*pi
               xlab <- 'Hour'
           },
           'month' = {
               x$PLOTBIN <- month(x$UTC)
               lims <- c(.5, 12.5)
               breaks <- seq(from=1, to=12, by=1)
               pStart <- -(.5/12)*2*pi
               xlab <- 'Month'
           }
    )
    plotData <- group_by(x, PLOTBIN) %>% 
        summarise(nDetections = sum(!is.na(call)),
                  nTotal = n(), 
                  pctDetections = nDetections/nTotal)
    gPlot <- plotData %>% 
        ggplot() +
        geom_bar(aes(x=PLOTBIN, y=pctDetections, fill=pctDetections), stat='identity') +
        coord_polar(start=pStart) +
        scale_fill_viridis_c() +
        scale_x_continuous(breaks=breaks, limits=lims, expand=c(0, 0)) + 
        ggtitle(title) +
        labs(x=xlab) 
    # list(plot=gPlot, data=plotData)
    gPlot
}


plotDataSimple<-function(x,effortBufferDays = 5, by_var = NULL){
    
    # Function to plot simplified data and effort using ggplot and facet wrap this 
    # is intended for use with single deployments where a full year of effort
    # occludes the output. This is intended to be used with data (x = binned
    # detections) that have already been cleaned. For instance, as single Morro
    # Bay deployment. 'by_var' is the facet_wrap variable and effortBufferDays
    # is the number of days to pad the plot with for your own taste
    barPosition='stack'
    x <- markNumEffort(x, keepCols=c('species', 'call'))
    effort <- bind_rows(lapply(split(x$effort, x$effort$year), function(y) {
        if(nrow(y) <= 1) {
            # print(z)
            return(NULL)
        }
        shortenOffs(y, nMax=364)
    })
    )
    data <- x$data
    
    #KJP edit
    effortdataout = subset(effort, 
                           binDate> (min(x$data$UTC)-period(effortBufferDays, 'day')) &
                               binDate<(max(x$data$UTC)+period(effortBufferDays, 'day')))
    
    # joint the data and the labels for plotting
    data <- left_join(data, effortdataout[c('plotX', 'binDate', 'nEffort')],
                      by=join_by(binDate))
    
    p<-ggplot() +
        geom_bar(data =data, aes(x=plotX), position=barPosition, width=1)
    
    
    # Create the label list and tick locations, format to date
    labs <- list(ix = seq(from=min(effortdataout$plotX), 
                          to=max(effortdataout$plotX), length.out=5),
                 label = seq(from=min(effortdataout$binDate), 
                             to=min(effortdataout$binDate), length.out=5))
    labs$label <- format(labs$label, '%b-%d')
    
    
    # Add the updated scale and scale limits
    p<-p+
        geom_path(data=effortdataout, aes(x=plotX, y=nEffort)) +
        theme_bw()+
        scale_x_continuous(limits=c(min(effortdataout$plotX),
                                    max(effortdataout$plotX)), 
                           breaks=labs$ix, labels=labs$label, name=NULL)
    
    # If you want a facet wrap, then use the 'by_var' col
    if(!is.null(by_var)){(p =p+ facet_wrap(~ .data[[by_var]]))}
    return(list(p))
    
}

getDepDetails <- function(x) {
    con <- dbConnect(x, drv=SQLite())
    on.exit(dbDisconnect(con))
    data <- dbReadTable(con, 'deploymentData')
    for(col in c('Start', 'End', 'DataStart', 'DataEnd')) {
        data[[col]] <- as.POSIXct(data[[col]], format='%Y-%m-%d %H:%M:%S', tz='UTC')
    }
    data
}

markInData <- function(x, depDet) {
    x <- split(x, x$DriftName)
    bind_rows(lapply(x, function(d) {
        # browser()
        thisDep <- depDet[depDet$DriftName %in% d$DriftName[1], ]
        start <- ifelse(is.na(thisDep$DataStart), thisDep$Start, thisDep$DataStart)
        end <- ifelse(is.na(thisDep$DataEnd), thisDep$End, thisDep$DataEnd)
        d$inData <- d$UTC >= start &
            d$UTC <= end
        d
    }))
}

markEffort <- function(x, effort, by=NULL, mark=TRUE) {
    if(!is.null(by)) {
        if(!by %in% colnames(x) ||
           !by %in% colnames(effort)) {
            stop('"by" must be in both dataframes')
        }
        return(bind_rows(lapply(split(x, x[[by]]), function(d) {
            markEffort(d, effort=effort[effort[[by]] == d[[by]][1], ], by=NULL, mark=mark)
        })))
    }
    # if we don't have this yet, assume that all start as
    # the opposite of what we are marking otherwise there's
    # no point in marking
    if(!'onEffort' %in% colnames(x)) {
        x$onEffort <- !mark
    }
    for(i in 1:nrow(effort)) {
        toMark <- x$UTC >= effort$start[i] &
            x$UTC <= effort$end[i]
        x$onEffort[toMark] <- mark
    }
    x
}

markRegion <- function(x) {
    # mby <- c('MBY', 'MOB', 'SF', 'SFB')
    # hum <- c('HUM', 'Crescent City')
    # ore <- c('ORE')
    # x[x %in% mby] <- 'MBY'
    # x[x %in% hum] <- 'HUM'
    # x[x %in% ore] <- 'ORE'
    # annes codes
    MorroBay<-c('MBY','MOB')
    Humboldt<-c('Crescent City','HUM', 'MND')
    Oregon<-c('ORE', 'COL')
    SanFrancisco<-c('HMB','SF','SFB')
    x[x %in% MorroBay] <- 'MorroBay'
    x[x %in% Humboldt] <- 'Humboldt'
    x[x %in% Oregon] <- 'Oregon'
    x[x %in% SanFrancisco] <- 'SanFrancisco'
    # factor(x, levels=c('Oregon', 'Humboldt', 'SanFrancisco','MorroBay'))
    x
}
# separate forcing for pascal/cces to align with adrift
markPCRegion <- function(x) {
    isHum <- c('CCES_007', 'PASCAL_002')
    isSf <- c('CCES_010', 'PASCAL_003')
    isMb <- c('PASCAL_004', 'PASCAL_021')
    isOregon <- c('PASCAL_001')
    x$region <- NA
    x$region[grepl('ADRIFT', x$DriftName)] <- markRegion(x$DeploymentSite[grepl('ADRIFT', x$DriftName)])
    x$region[x$DriftName %in% isHum] <- 'Humboldt'
    x$region[x$DriftName %in% isSf] <- 'SanFrancisco'
    x$region[x$DriftName %in% isOregon] <- 'Oregon'
    x$region[x$DriftName %in% isMb] <- 'MorroBay'
    x$region <- factor(x$region, levels=c('Oregon', 'Humboldt', 'SanFrancisco','MorroBay'))
    x
}

markSeason <- function(x) {
    season <- c(rep('Winter', 2),
                rep('Upwelling', 4),
                rep('Post-Upwelling', 5),
                'Winter')
    factor(season[month(x)], levels=c('Upwelling', 'Post-Upwelling', 'Winter'))
}

plotRegionSeason <- function(x, PLOTFUN, title=NULL,  ...) {
    if(!all(c('DeploymentSite', 'UTC') %in% colnames(x))) {
        stop('Must have "DeploymentSite" and "UTC" columns')
    }
    # x$Region <- markRegion(x$DeploymentSite)
    x <- markPCRegion(x)
    x <- rename(x, Region=region)
    if(!'Season' %in% colnames(x)) {
        x$Season <- markSeason(x$UTC)
    }
    byRegion <- split(x, x$Region)
    plots <- vector('list', length=length(byRegion))
    names(plots) <- names(byRegion)
    
    for(i in seq_along(byRegion)) {
        bySeason <- split(byRegion[[i]], byRegion[[i]]$Season)
        thisPlots <- vector('list', length=length(byRegion))
        for(j in seq_along(bySeason)) {
            thisRegion <- names(byRegion)[i]
            thisSeason <- names(bySeason)[j]
            thisPlots[[j]] <- PLOTFUN(bySeason[[j]], region=thisRegion, season=thisSeason, ...)
        }
        plots[[i]] <- reduce(thisPlots, `|`)
    }
    result <- reduce(plots, `/`) + plot_layout(nrow=length(plots), ncol=1)
    if(!is.null(title)) {
        result <- result + plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=0.5)))
    }
    result
}

loadNoiseLog <- function(x, project=NULL) {
    x <- read_xlsx(x)
    if('Drift#' %in% colnames(x)) {
        x <- rename(x, 'Drift' = 'Drift#')
    }
    x <- rename(x, 'start' = 'Start time', 'end' = 'End time')
    x$DriftName <- paste0(project, '_', formatNumber(x$Drift))
    x
}

loadSpecAnno <- function(x) {
    result <- readSpecAnno(x)
    if(is.null(result) || nrow(result) == 0) {
        return(NULL)
    }
    result <- select(result, all_of(c('start', 'end', 'eventType', 'Note')))
    result <- rename(result, 'UTC' = 'start', 'call' = 'eventType')
    result
}

getPgEvent <- function(db, grouping=c('event', 'detGroup', 'clickTrain'), label=NULL, extraCols = NULL) {
    # Combine all click/event tables, even by diff detector. Binary will have det name
    con <- dbConnect(SQLite(), db)
    on.exit(dbDisconnect(con))
    tables <- dbListTables(con)
    # Read in event data from either offlineclicks/events or detection
    # group localiser. Click version has common naming convention,
    # det group does not so we have to go look it up. If we are just
    # reading in all the data we only care about SA data
    if(is.null(grouping)) {
        grouping <- c('event', 'detGroup', 'clickTrain')
    }
    if(length(grouping) > 1) {
        
        if(length(label) == 1) {
            label <- rep(label, length(grouping))
        }
        return(
            suppressWarnings(bind_rows(
                lapply(seq_along(grouping), function(x) {
                    getPgEvent(db, grouping[x], label[x], extraCols)
                }))
            ))
    }
    switch(match.arg(grouping),
           'event' = {
               detTables <- grep('OfflineClicks', tables, value=TRUE)
               eventTables <- grep('OfflineEvents', tables, value=TRUE)
               if(is.null(label)) {
                   label <- 'eventType'
               }
               eventColumns <- c('Id', label, 'comment')
               evName <- 'OE'
           },
           'detGroup' = {
               
               dgNames <- PAMpal:::findModuleNames(con, 'Detection Group Localiser')
               
               eventTables <- dgNames
               detTables <- paste0(eventTables, '_Children')
               if(is.character(eventTables)) {
                   dglCols <- dbListFields(con, eventTables[1])
                   label <- PAMpal:::parseDglLabel(label, dglCols)
               } else {
                   label <- NULL
               }
               
               eventColumns <- unique(c('Id', label, 'Text_Annotation'))
               evName <- 'DGL'
           },
           'clickTrain' = {
               trainNames <- grep('Click_Train', tables, value=TRUE)
               eventTables <- trainNames[!grepl('Children', trainNames)]
               detTables <- trainNames[grepl('Children', trainNames)]
               label <- NULL
               eventColumns <- c('Id', label)
               evName <- 'CT'
           },
           {
               stop("I don't know how to group by ", grouping, '.\n', call.=FALSE)
           }
    )
    
    if(length(detTables)==0 ||
       length(eventTables)==0) {
        warning('Could not find event tables for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }
    allDetections <- bind_rows(
        lapply(detTables, function(table) {
            dt <- dbReadTable(con, table)
            if(is.null(dt) ||
               nrow(dt) == 0) {
                return(NULL)
            }
            dt
        })
    )
    if(nrow(allDetections)==0) {
        warning('No detections found for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }
    if('EventId' %in% colnames(allDetections)) {
        allDetections$parentID <- allDetections$EventId
    }
    # if(!'UID' %in% colnames(allDetections))
    
    allEvents <- bind_rows(
        lapply(eventTables, function(table) {
            et <- dbReadTable(con, table)
            if(is.null(et) ||
               nrow(et) == 0) {
                return(NULL)
            }
            et
        })
    )
    if(nrow(allEvents)==0) {
        warning('No events found for grouping method "', grouping,
                '" in database ', basename(db))
        return(NULL)
    }
    
    eventColumns <- eventColumns[eventColumns %in% colnames(allEvents)]
    allEvents <- select(allEvents, any_of(c(eventColumns, extraCols)))
    
    # Do i want all detections in clicks, or only all in events?
    # left_join all det, inner_join ev only
    if(!('Id' %in% names(allEvents)) ||
       !('parentID' %in% names(allDetections))) {
        message('Id and parentID columns not found in database ', basename(db),
                ', these are required to process data.')
        return(NULL)
    }
    
    allDetections <- inner_join(
        allDetections, allEvents, by=c('parentID'='Id'), relationship='many-to-one'
    )
    if(!('newUID' %in% colnames(allDetections))) {
        allDetections$newUID <- -1
    }
    allDetections <- allDetections %>%
        mutate(BinaryFile = str_trim(.data$BinaryFile),
               # UTC = as.POSIXct(as.character(UTC), format='%Y-%m-%d %H:%M:%OS', tz='UTC')) %>%
               UTC = PAMpal:::pgDateToPosix(.data$UTC)) %>%
        
        select(any_of(unique(c(eventColumns, 'UTC', 'UID', 'parentID', 'BinaryFile', 'newUID', extraCols))))
    
    # rename column to use as label - standardize across event group types
    colnames(allDetections)[which(colnames(allDetections)==label)] <- 'eventLabel'
    if(grouping == 'detGroup' &&
       length(eventColumns) > 2 &&
       'Text_Annotation' %in% colnames(allDetections)) {
        colnames(allDetections)[colnames(allDetections) == 'Text_Annotation'] <- 'comment'
    }
    
    if(!('eventLabel' %in% colnames(allDetections))) {
        allDetections$eventLabel <- NA
    }
    # if(doSR) {
    #   allDetections <- matchSR(allDetections, db, extraCols=c('SystemType'))
    # }
    
    # apply str_trim to all character columns
    whichChar <- which(sapply(allDetections, function(x) 'character' %in% class(x)))
    for(i in whichChar) {
        allDetections[, i] <- str_trim(allDetections[, i])
    }
    # allDetections <- select(allDetections, -.data$UTC)
    # allDetections <- dropCols(allDetections, 'UTC')
    # if(!'UID' %in%
    # allDetections$UID <- as.character(allDetections$UID)
    # allDetections$newUID <- as.character(allDetections$newUID)
    # allDetections$parentID <- paste0(evName, allDetections$parentID)
    # allDetections
    if('Sound_type' %in% colnames(allDetections)) {
        allDetections$call <- allDetections$Sound_type
    } else {
        allDetections$call <- NA
    }
    allDetections <- rename(allDetections, 'species' = 'eventLabel')
    select(allDetections, any_of(c('UTC', 'species', 'call', extraCols)))
}

loadRavenTable <- function(x) {
    if(!is.character(x) ||
       !file.exists(x)) {
        stop('Raven sources must be path to a Raven .txt file')
    }
    tbl <- read.table(x, header=TRUE, sep='\t', quote='\"')
    # if(grepl('ADRIFT_097', x)) {
    #   browser()
    # }
    if(is.null(tbl) || nrow(tbl) == 0) {
        return(NULL)
    }
    colnames(tbl) <- gsub('\\.', '', colnames(tbl))
    # if('BeginDateTime' %in% colnames(tbl)) {
    #   tbl$UTC <- as.POSIXct(tbl$BeginDateTime, format='%Y/%m/%d  %H:%M:%OS', tz='UTC')
    #   tbl$end <- tbl$UTC + tbl$EndTimes - tbl$BeginTimes
    # } else {
    #   tbl$UTC <- NA
    #   tbl$end <- NA
    # }
    # tbl$DeltaTimes <- round(tbl$DeltaTimes, 3)
    # tbl$BeginTimes <- round(tbl$BeginTimes, 3)
    # out <- data.frame(UTC = fileTime + tbl[['BeginTimes']],
    #                   Duration = tbl[['DeltaTimes']],
    #                   f1 = tbl[['LowFreqHz']],
    #                   f2 = tbl[['HighFreqHz']],
    #                   Label = tbl[['Annotation']])
    # distinct(out)
    
    tbl$species <- tbl$Species
    tbl$call <- NA
    tbl[c('species', 'call', 'BeginTimes', 'EndTimes')]
}

getDriftStart <- function(dir) {
    driftDirs <- list.dirs(dir, recursive=FALSE, full.names=TRUE)
    isDrift <- grepl('^[A-Z]+_[0-9]+.*', basename(driftDirs))
    driftDirs <- driftDirs[isDrift]
    driftDirs <- driftDirs[!grepl('CCES_014_HF', driftDirs)]
    driftName <- gsub('([A-Z]+_[0-9]+)_.*$', '\\1', basename(driftDirs))
    driftStarts <- vector('list', length=length(driftName))
    names(driftStarts) <- driftName
    pb <- txtProgressBar(min=0, max=length(driftStarts), style=3)
    for(i in seq_along(driftStarts)) {
        cat(driftName[i])
        # if(grepl('CCES_014_HF', driftDirs[i])) {
        #   next
        # }
        wavFiles <- list.files(driftDirs[i], pattern='wav$', full.names=FALSE)
        if(length(wavFiles) == 0 ||
           driftName[i] == 'CCES_017') {
            # wavFiles <- list.files(driftDirs[i], pattern='wav$', full.names=TRUE, recursive=TRUE)
            switch(driftName[i],
                   'PASCAL_009' = {
                       # wavFiles <- wavFiles[grepl('288kHz', wavFiles)]
                       wavFiles <- list.files(file.path(driftDirs[i], 'Soundtrap-G_288kHz'), 
                                              full.names=FALSE,
                                              pattern='wav$')
                   },
                   'PASCAL_024' = {
                       # wavFiles <- wavFiles[grepl('Synch', wavFiles)]
                       wavFiles <- list.files(file.path(driftDirs[i], 'Soundtrap-F_SynchCorrected_288kHz'), 
                                              full.names=FALSE,
                                              pattern='wav$')
                   },
                   'PASCAL_026' = {
                       # wavFiles <- wavFiles[grepl('Synch', wavFiles)]
                       wavFiles <- list.files(file.path(driftDirs[i], 'Soundtrap_H_SynchCorrected'), 
                                              full.names=FALSE,
                                              pattern='wav$')
                   },
                   'PASCAL_029' = {
                       # wavFiles <- wavFiles[grepl('Synch', wavFiles)]
                       wavFiles <- list.files(file.path(driftDirs[i], 'Soundtrap_H_SynchCorrected'), 
                                              full.names=FALSE,
                                              pattern='wav$')
                   },
                   'CCES_017' = {
                       wavFiles <- list.files(driftDirs[i],
                                              recursive=TRUE,
                                              full.names=FALSE,
                                              pattern='wav$')
                   },
                   warning('No plan for ', driftName[i])
            )
            wavFiles <- basename(wavFiles)
        }
        wavDates <- wavToTime(wavFiles)
        driftStarts[[i]] <- as.POSIXct(min(wavDates, na.rm=TRUE), origin='1970-01-01', tz='UTC')
        setTxtProgressBar(pb, value=i)
    }
    driftStarts
}

wavToTime <- function(x) {
    x <- basename(x)
    if(length(x) > 1) {
        return(sapply(x, wavToTime))
    }
    format <- c('pamguard', 'pampal', 'soundtrap', 'sm3', 'icListens1', 'icListens2')
    for(f in format) {
        switch(
            f,
            'pamguard' = {
                date <- gsub('.*([0-9]{8}_[0-9]{6}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 15), tz = 'UTC', format = '%Y%m%d_%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 17, 19)) / 1e3
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            },
            'pampal' = {
                date <- gsub('.*([0-9]{14}_[0-9]{3})\\.wav$', '\\1', x)
                posix <- as.POSIXct(substr(date, 1, 14), tz = 'UTC', format = '%Y%m%d%H%M%S')
                if(is.na(posix)) next
                millis <- as.numeric(substr(date, 16, 18)) / 1e3
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            },
            'soundtrap' = {
                date <- gsub('.*\\.([0-9]{12})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            },
            'sm3' = {
                date <- gsub('.*\\_([0-9]{8}_[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d_%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            },
            'icListens1' = {
                date <- gsub('.*_([0-9]{8}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%Y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            },
            'icListens2' = {
                date <- gsub('.*_([0-9]{6}-[0-9]{6})\\.wav$', '\\1', x)
                posix <- as.POSIXct(date, format = '%y%m%d-%H%M%S', tz='UTC')
                millis <- 0
                if(!is.na(posix)) {
                    FOUNDFORMAT <<- f
                    break
                }
            }
        )
    }
    posix + millis
}

compareLizData <- function(ours, liz, effort, gps, keepSpecies) {
    if(length(keepSpecies) ==  1) {
        keepSpecies <- rep(keepSpecies, 2)
    }
    ours <- filter(ours, species == keepSpecies[1])
    liz <- filter(liz, species == keepSpecies[2])
    ours$call <- NA
    liz$call <- NA
    ourBin <- formatBinnedPresence(ours, effort=effort, bin='hour',
                                   gps=gps)
    lizBin <- formatBinnedPresence(liz, effort=effort, bin='hour',
                                   gps=gps)
    
    ourBin <- bind_rows(lapply(split(ourBin, ourBin$DriftName), function(x) {
        x$missing <- FALSE
        thisLiz <- lizBin[lizBin$DriftName == x$DriftName[1], ]
        if(nrow(thisLiz) == 0) {
            return(x)
        }
        
        lizHours <- thisLiz$UTC[!is.na(thisLiz$species)]
        oursHours <- x$UTC[!is.na(x$species)]
        newLiz <- !lizHours %in% oursHours
        if(sum(newLiz) == 0) {
            return(x)
        }
        newHours <- lizHours[newLiz]
        x$missing[x$UTC %in% newHours] <- TRUE
        x
    }))
    oursHours <- sum(!is.na(ourBin$species))
    lizHours <- sum(!is.na(lizBin$species))
    nMissed <- sum(ourBin$missing)
    cat('Our Hours:', (oursHours))
    cat('\nLiz Hours:', (lizHours))
    cat('\nNew Liz:', nMissed)
    cat('\nLiz Percent:', round(100*nMissed / (nMissed + oursHours), 1))
    invisible(ourBin)
}

lizBmLabeler <- function(x) {
    bind_rows(lapply(split(x, x$Comments), function(d) {
        result <- NULL
        if(grepl('D.*[Cc]alls?', d$Comments[1])) {
            result <- bind_rows(result,
                                mutate(d, call='D'))
        }
        if(grepl('A.*calls', d$Comments[1])) {
            result <- bind_rows(result,
                                mutate(d, call='A NE Pacific'))
        }
        if(grepl('B.*calls', d$Comments[1])) {
            result <- bind_rows(result,
                                mutate(d, call='B NE Pacific'))
        }
        if(is.null(result)) {
            return(mutate(d, call=NA))
        }
        result
    }
    ))
}

plotDriftTracks <- function(drift, etopo = 'etopo180.nc', filename=NULL, 
                            bathy=TRUE, sl=TRUE, wca=TRUE, nms=FALSE,
                            season=FALSE, 
                            size = 4, xlim=1, ylim=.5, 
                            labelBy='DriftName', title=NULL,
                            dataPath='data/map') {
    if(is.null(etopo)) {
        etopo <- file.path(dataPath, 'etopo180.nc')
    } else {
        etopo <- file.path(dataPath, etopo)
    }
    rangeDf <- data.frame(Longitude=c(-135, -108), Latitude=c(20, 55),
                          Depth=c(0, 200))
    
    if(is.null(xlim)) {
        xlim <- range(drift$Longitude)
    }
    if(is.null(ylim)) {
        ylim <- range(drift$Latitude)
    }
    if(length(xlim) == 1) {
        xlim <- range(drift$Longitude) + c(-1, 1) * xlim
    }
    if(length(ylim) == 1) {
        ylim <- range(drift$Latitude) + c(-1, 1) * ylim
    }
    if(xlim[2] < rangeDf$Longitude[1] ||
       xlim[1] > rangeDf$Longitude[2] ||
       ylim[2] < rangeDf$Latitude[1] ||
       ylim[1] > rangeDf$Latitude[2]) {
        cat('\nPlot is entirely out of range: ',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> '), ' vs. (-135 -> -108, 20 -> 55)',
            sep='')
        return(NULL)
    }
    xlim[xlim < rangeDf$Longitude[1]] <- rangeDf$Longitude[1]
    xlim[xlim > rangeDf$Longitude[2]] <- rangeDf$Longitude[2]
    ylim[ylim < rangeDf$Latitude[1]] <- rangeDf$Latitude[1]
    ylim[ylim > rangeDf$Latitude[2]] <- rangeDf$Latitude[2]
    
    bathyData <- as.bathy(raster::raster(etopo))
    
    bathyData <- try(subsetBathy(bathyData, x=xlim, y=ylim, locator = FALSE), silent=TRUE)
    if(inherits(bathyData, 'try-error')) {
        cat('\nBathymetry subset failed for coordinates:',
            paste0(xlim, collapse=' -> '), ', ',
            paste0(ylim, collapse=' -> ')
        )
        return(NULL)
    }
    wid <- nrow(bathyData)
    xmar <- 1.24
    ht <- ncol(bathyData)
    ymar <- 1.84
    if(wid <= ht) {
        width <- size
        height <- size * ht / wid
    } else {
        height <- size
        width <- size * wid / ht
    }
    if(!is.null(filename)) {
        tryOpen <- suppressWarnings(try(
            png(filename, height = height + ymar, width = width + xmar, units='in',res=300)
        ))
        if(inherits(tryOpen, 'try-error')) {
            warning('Unable to create image "', filename, '", file appears to be open already')
            return(NULL)
        }
        on.exit(dev.off())
    }
    # Setting up bathy data and legend
    if(bathy) {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), 0, "darkblue", "lightblue"))
        bVals <- round(seq(from=0, to=abs(min(bathyData)), length.out=7), 0)
        bVals[c(2,4,6)] <- NA
        bCols <- colorRampPalette(c('lightblue', 'darkblue'))(7)
    } else {
        depthPal <- list(c(0, max(bathyData), grey(.7), grey(.9), grey(.95)),
                         c(min(bathyData), -200, "skyblue1", 'skyblue1'),
                         c(-200, 0,"lightblue", "lightblue"))
        bVals <- round(c(0, NA, 200, NA, abs(min(bathyData))), 0)
        bCols <- c(rep('lightblue', 3), rep('skyblue1', 2))
    }
    
    plot(bathyData, image = TRUE, land = TRUE, axes = T, lwd=0.3,
         bpal = depthPal, lty=1,
         shallowest.isobath=-100, deepest.isobath=-200, step=100, drawlabels=T)
    title(main=title)
    # SHipping lanes
    if(sl) {
        shipLanes <- readRDS(file.path(dataPath, 'ShippingLaneCA.RData'))
        # shipLanes <- readRDS('../Data/SPOTXML/ShippingLaneCA.RData')
        plot(shipLanes$geometry, add=TRUE, border='orange', lwd=2)
    }
    # Wind Call Areas
    if(wca) {
        windCall <- readRDS(file.path(dataPath, 'WindCallBoundary.RData'))
        # windCall <- readRDS('../Data/SPOTXML/WindCallBoundary.RData')
        plot(st_union(windCall), add=TRUE, border='purple', lwd=2)
    }
    # national marine sanctuaries
    if(nms) {
        nmsFiles <- list.files(dataPath, pattern='NMS', full.names=TRUE)
        nmsData <- lapply(nmsFiles, readRDS)
        nmsData <- do.call('c', lapply(nmsData, function(n) n$geometry))
        plot(nmsData, add=TRUE, border='blue', lwd=2)
    }
    myScaleBathy(bathyData, deg=diff(xlim) * .2, x= 'bottomleft', inset=5, col='white')
    # Plotting drift and start/end points
    for(d in seq_along(unique(drift[[labelBy]]))) {
        thisDrift <- drift[drift[[labelBy]] == unique(drift[[labelBy]])[d], ]
        if(nrow(thisDrift) == 1) next
        thisDrift <- arrange(thisDrift, UTC)
        if(isTRUE(season)) {
            baseCol <- seasonColors[as.character(thisDrift$season[1])]
        } else {
            baseCol <- 'black'
        }
        lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col=baseCol, lwd=2)
        if(isTRUE(bathy)) {
            lines(x=thisDrift$Longitude, y=thisDrift$Latitude, col='grey', lwd=1, lty=5)
        }
        
    # }
    # if(isTRUE(season)) {
    #     legend(x='topright', legend=names(seasonColors), col=seasonColors,
    #            lwd=4, cex=0.7, title='Deployment Season')
    }
    # if(isTRUE(simple)) {
    if(is.null(filename)) {
        return(invisible(drift))
    }
    return(invisible(filename))
    # }
    #### Below cutoff from simple version ####
    # Plot port cities and other POI
    text(x=poiDf$Longitude, y=poiDf$Latitude, labels=poiDf$Name, cex=.6, srt=30, adj=c(-.1, .5))
    points(x=poiDf$Longitude, y=poiDf$Latitude, cex=.5, pch=16)
    
    # Legendary
    lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
                         col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
                         seg.len = 1, cex=1, plot=FALSE)
    useCex <- min(.2 * diff(xlim) / lastLegend$rect$w, 1)
    lastLegend <- legend(x='topright', legend=c('Start', 'End', unique(drift[[labelBy]])),
                         col=c('black', 'grey', rep('black', length(mapLabs))), pch=c(15, 17, mapLabs), merge=FALSE,
                         seg.len = 1, cex=useCex)
    
    if(bathy) {
        lastLegend <- myGradLegend(lastLeg=lastLegend, vals=bVals, cols=bCols,
                                   title='      Depth (m)', cex=useCex, tCex=.67)
    }
    if(is.null(filename)) {
        return(invisible(drift))
    }
    invisible(filename)
}

myScaleBathy <- function (mat, deg = 1, x = "bottomleft", y = NULL, inset = 10,
                          angle = 90, ...) {
    usr = par("usr")
    if (is.numeric(x) == TRUE & is.null(y) == FALSE) {
        X <- x
        Y <- y
        lat <- abs(Y)
    }
    if (is.numeric(x) == FALSE & is.null(y) == TRUE) {
        insetx = abs((usr[2] - usr[1]) * inset/100)
        insety = abs((usr[4] - usr[3]) * inset/100)
        X <- switch(x, bottomright = (usr[2] - insetx - deg),
                    topright = (usr[2] - insetx - deg), bottomleft = (usr[1] +
                                                                          insetx), topleft = (usr[1] + insetx))
        Y <- switch(x, bottomright = (usr[3] + insety), topright = (usr[4] -
                                                                        insety), bottomleft = (usr[3] + insety), topleft = (usr[4] -
                                                                                                                                insety))
        lat <- switch(x, bottomright = abs(min(as.numeric(colnames(mat)))),
                      topright = abs(max(as.numeric(colnames(mat)))),
                      bottomleft = abs(min(as.numeric(colnames(mat)))),
                      topleft = abs(max(as.numeric(colnames(mat)))))
    }
    cos.lat <- cos((2 * pi * lat)/360)
    perdeg <- (2 * pi * (6372.798 + 21.38 * cos.lat) * cos.lat)/360
    arrows(X, Y, X + (deg), Y, code = 3, length = 0.05, angle = angle, lwd=2, ...)
    text((X + X + (deg))/2, Y, adj = c(0.5, -0.5), labels = paste(round(perdeg *
                                                                            deg, 0), "km"), ...)
}

seasonColors <- c(brewer.pal(n=9, 'Blues')[8], 
                  brewer.pal(n=9, 'Greens')[6],
                  brewer.pal(n=12, 'Set3')[c(12)])
names(seasonColors) <- c('Winter', 'Upwelling', 'Post-Upwelling')
# usedSeasonColors <- names(seasonColors) %in% thisDrift$season#Shannon
# locs if we want to add to map
poiDf <- tibble::tribble(
    ~Name, ~Latitude, ~Longitude,
    "San Diego", 32.71068967391705, -117.17147162885448,
    "Santa Barbara",34.407205041229595, -119.69269808900013,
    'Ventura',34.250263795418434, -119.26720606042934,
    'Morro Bay',35.36841835524968, -120.86325292077103,
    'Monterey Bay',36.604218252060306, -121.89240128825472,
    'Santa Cruz',36.96225980624226, -122.00212520928149,
    'Half Moon Bay',37.50293801397416, -122.48765584637566,
    'San Francisco',37.813095434735914, -122.50037485469521,
    'Bodega Bay',38.30982199529412, -123.05611099859385,
    'Fort Bragg',39.42849849826603, -123.81309923266699,
    'Shelter Cove',40.02403071506129, -124.06607534915634,
    'Eureka',40.806299478998056, -124.1807826182425,
    'Crescent City',41.74692081633374, -124.19223894744171,
    'Point Conception',34.4483491615287, -120.47193766943991,
    'Point Arena',38.91093890217707, -123.71170879559632,
    'Cape Mendocino', 40.438268949326925, -124.40971460611878
)

plotModAcousticScene<-function(x,datebreaks,typeCol='species',freqMap){
    x$plotStart <- floor_date(x$UTC, unit = 'hour')
    x$plotEnd <- x$plotStart +hours(1)
    
    for(n in 1:nrow(x)-1){
        x$timegap[n]=as.numeric(difftime(x$plotStart[n],x$plotStart[n+1],units="days"))
    }
    
    missType <- unique(x[[typeCol]])[!unique(x[[typeCol]]) %in% 
                                         freqMap$type]
    if (length(missType) > 0) {
        warning(length(missType), " types in \"x$", typeCol, 
                "\" were not in \"freqMap$type\" (", paste0(missType, 
                                                            collapse = ", "), "), they will not be plotted.", 
                call. = FALSE)
        x <- x[!x[[typeCol]] %in% missType, ]
    }
    x$TEMPJOINCOLUMN <- x[[typeCol]]
    x <- left_join(x, freqMap, by = join_by("TEMPJOINCOLUMN" == 
                                                "type"))
    x$TEMPJOINCOLUMN <- NULL
    x <- x[!is.na(x[["freqMin"]]), ]
    x <- distinct(x[c("plotStart", "plotEnd", "freqMin", "freqMax", 
                      typeCol)])
    if (is.null(freqMin)) {
        freqMin <- min(x[["freqMin"]])
    }
    if (freqMin < 1 && scale == "log10") {
        freqMin <- 1
    }
    
    g<-ggplot(x) +
        geom_rect(aes(xmin=plotStart,xmax=plotEnd,ymin=freqMin,
                               ymax=freqMax,fill=species)) +
        scale_x_break(datebreaks,scales='free',expand=FALSE) + 
        scale_x_datetime(date_labels='%b',date_breaks='1 month') +
        theme(axis.text.x.top=element_blank()) +
        # scale_y_log10(limits=c(NA,120000)) +
        ylab('Frequency (Hz)')
    g <- PAMscapes:::myLog10Scale(g, range=c(min(freqMap$freqMin), max(freqMap$freqMax)), dim='y')
    
    if('color' %in% colnames(freqMap)) {
        colNames <- freqMap[['color']]
        names(colNames) <- freqMap[['type']]
        g <- g +
            scale_fill_manual(values=colNames)
    }
    g
}


plotPairedScene <- function(data, gps, drift='ADRIFT_001', freqMap, freqMin=10, bin='1hour',
                            alpha=1, title=NULL, bathyFile='data/map/etopo180.nc') {
    data <- filter(data, DriftName %in% drift)
    gps <- filter(gps, DriftName %in% drift, recordingEffort)
    timePad <- switch(bin,
                      '1hour' = 4*3600,
                      '1day' = 86400
    )
    scene <- plotAcousticScene(data, freqMap=freqMap, freqMin=freqMin, bin=bin, alpha=alpha) +
        xlim(range(gps$UTC) + c(-1, 1) * timePad)
    scene <- PAMscapes:::myLog10Scale(scene, c(min(freqMap$freqMin), max(freqMap$freqMax)), dim='y')
    pathTime <- data.frame(UTC=seq(from=min(gps$UTC), to=max(gps$UTC), by=3600),
                           freq=11)
    scene <- scene +
        geom_path(data=pathTime, aes(x=UTC, y=freq, col=UTC), lwd=2) +
        scale_color_gradientn(colors=viridisLite::viridis(32, option='B'), trans='time', guide='none')
    bathy <- as.bathy(raster::raster(bathyFile))
    bathy <- fortify.bathy(bathy)
    
    xPad <- .25
    xRange <- diff(range(gps$Longitude)) + xPad*2
    yRange <- diff(range(gps$Latitude))
    yPad <- ((2.5*xRange) - yRange)/2
    # yPad <- 1.25
    # nX <- unique(bathy$x)
    # nY <- unique(bathy$y)
    # nX <- nX[nX > (min(gps$Longitude)-xPad) &
    #              nX < (max(gps$Longitude) + xPad)]
    bathy <- bathy %>% 
        filter(x > (min(gps$Longitude)-xPad),
               x < (max(gps$Longitude) + xPad),
               y > (min(gps$Latitude) - yPad),
               y < (max(gps$Latitude) + yPad)) %>% 
        mutate(depth=cut(z, c(-Inf, -200, 0, Inf)))
    
    tracks <- ggplot() +
        geom_tile(data=filter(bathy),
                  aes(x=x, y=y, fill=depth)) +
        geom_contour(data=filter(bathy, z < 0),
                     aes(x=x, y=y, z=z), breaks=c(-200, -100),col='black') +
        scale_fill_manual(values=c('lightblue', 'skyblue1', 'lightgray'), 
                          guide='legend') +
        geom_path(data=gps, aes(x=Longitude, y=Latitude, color=UTC, group=DriftName), lwd=1) +
        coord_quickmap(expand=c(0)) +
        scale_color_gradientn(colors=viridisLite::viridis(32, option='B'), trans='time') +
        guides(fill='none',
               color=guide_colorbar(position='inside')) +
        theme(legend.position.inside=c(1,1),
              legend.justification.inside=c(1,1),
              legend.background=element_rect(color='black'), 
              legend.key.width=unit(.4, 'cm'),
              legend.text=element_text(size=8)) +#, axis.title.x=element_blank())
        labs(x='Longitude', y='Latitude')
    list(scene, tracks)
    tracks + scene + plot_layout(widths=c(1,3)) + 
        plot_annotation(title=title, theme=theme(plot.title=element_text(hjust=.5)))
    # tracks
}

plotTrackDetections <- function(data, gps, drift='ADRIFT_001', bin='hour',
                                plotSpecies=NULL, 
                                alpha=1, title=NULL, bathyFile=here('data/map/etopo180.nc')) {
    data <- filter(data, DriftName %in% drift)
    if(!is.null(plotSpecies)) {
        data <- filter(data, .data$species %in% plotSpecies)
    }
    gps <- filter(gps, DriftName %in% drift, recordingEffort)
    
    data <- formatBinnedPresence(data, gps=gps, bin=bin)
    data <- filter(data, !is.na(species))
    
    bathy <- as.bathy(raster::raster(bathyFile))
    bathy <- fortify.bathy(bathy)
    
    xPad <- .5
    yPad <- .5
    bathy <- bathy %>% 
        filter(x > (min(gps$Longitude) - xPad),
               x < (max(gps$Longitude) + xPad),
               y > (min(gps$Latitude) - yPad),
               y < (max(gps$Latitude) + yPad)) %>% 
        mutate(depth=cut(z, c(-Inf, -200, 0, Inf)))
    startEnd <- gps %>% 
        arrange(UTC) %>% 
        group_by(DriftName) %>% 
        slice(c(1, n())) %>% 
        ungroup()
    startEnd$StartEnd <- factor(rep(c('Start', 'End'), nrow(startEnd)/2), 
                                levels=c('Start', 'End'))
    
    tracks <- ggplot() +
        geom_tile(data=filter(bathy),
                  aes(x=x, y=y, fill=depth)) +
        geom_contour(data=filter(bathy, z < 0),
                     aes(x=x, y=y, z=z), breaks=c(-200, -100),col='black') +
        scale_fill_manual(values=c('lightblue', 'skyblue1', 'lightgray'), 
                          guide='legend') +
        geom_path(data=gps, aes(x=Longitude, y=Latitude, group=DriftName), lwd=1) +
        geom_point(data=data, aes(x=Longitude, y=Latitude, col=species)) +
        geom_point(data=startEnd, aes(x=Longitude, y=Latitude, shape=StartEnd), size=3) +
        scale_shape_manual(values=c(15, 17), guide=guide_legend(order=1)) +
        coord_quickmap(expand=c(0)) +
        guides(fill='none') +
        labs(x='Longitude', y='Latitude') +
        ggtitle(title)
    tracks
}

