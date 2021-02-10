#' GenDG Calculates the Das Gupta estimates and Persons Per Household 
#'  and provides a plotly chart for a specific county and decade
#'
#' @param listID the list containing place id and Place names
#' @param HUData Input Housing Unit data --Stored as an excel file on the local drive
#' @param POPData Input Household Population and PPH data  --Stored as an excel file on the local drive
#' @param StYear The decade identifier, 1990 for 1990-, etc.  Currently a constant, but will become a variable
#' @return plotly graphic and data file
#' @export


GenDG <- function(listID, type,  DGList) {  #Currently set to 1990-2000 will become a variable in a future version

  ctyfips <- listID$ctyNum
  ctyname <- listID$ctyName
  plfips <- listID$plNum
  plname <- listID$plName
  
  # These datafiles are taken from the HistoricalFinalTables.accdb data base.  indata90_00 is in a query.

  indata90_00 <- read_excel("data/City and County 1990 to 2000 Internal.xlsx", sheet=1)
  indata00_10 <- read_excel("data/CityandCounty_Estimates_MasterV2009.xlsx", sheet = 1)
  
# Selecting range of data variable by the type value
if(type == "Total Housing Units") {
    varSel90_00 <- c("CountyFIPS","PlaceFIPS","Thu90", "Thu91", "Thu92", "Thu93", "Thu94", "Thu95",
                     "Thu96", "Thu97", "Thu98", "Thu99", "Thu00", "Thu00x")
    varSel00_10 <- c("CountyFIPS","PlaceFIPS","Thu00", "Thu01", "Thu02", "Thu03", "Thu04", "Thu05",
                     "Thu06", "Thu07", "Thu08", "Thu09", "Thu10", "Thu10x")
    prefix  <- "Thu"
}  
  
  if(type == "Population") {
    varSel90_00 <- c("CountyFIPS","PlaceFIPS","Tp90", "Tp91", "Tp92", "Tp93", "Tp94", "Tp95",
                     "Tp96", "Tp97", "Tp98", "Tp99", "Tp00", "Tp00x")
    varSel00_10 <- c("CountyFIPS","PlaceFIPS","Tp00", "Tp01", "Tp02", "Tp03", "Tp04", "Tp05",
                     "Tp06", "Tp07", "Tp08", "Tp09", "Tp10", "Tp10x")
    prefix <- "Tp"
  }  
  
  if(type == "Persons Per Household") {
    varSel90_00 <- c("CountyFIPS","PlaceFIPS","Pph90", "Pph91", "Pph92", "Pph93", "Pph94", "Pph95",
                     "Pph96", "Pph97", "Pph98", "Pph99", "Pph00", "Pph00x")
    varSel00_10 <- c("CountyFIPS","PlaceFIPS","Pph00", "Pph01", "Pph02", "Pph03", "Pph04", "Pph05",
                     "Pph06", "Pph07", "Pph08", "Pph09", "Pph10", "Pph10x")
    prefix <- "Pph"
  }    
# Reading County Total Housing Units data, with one year projection and Census estimate
if(plfips == "") { 
# 1990-2000
if(ctyfips != "014") { 

f.ctyDG90 <- indata90_00 %>%
  select(varSel90_00) %>%
  filter(CountyFIPS == ctyfips) %>% filter(PlaceFIPS == "00000") %>%
  gather(HU, value, varSel90_00[3]:varSel90_00[14], factor_key=TRUE) %>%
  mutate(Variable = ifelse(HU == varSel90_00[14],"P_10", sub(prefix,"",HU)))

f.ctyConst90 <- f.ctyDG90 %>% filter(Variable %in% c("90","P_10","00")) %>%
  select(CountyFIPS, Variable, value)  %>%
  spread(Variable, value) %>% 
  rename(Q_10 = "00",P_0 = "90")

f.ctyYr90 <- f.ctyDG90 %>% filter(Variable != "P_10") %>%
  mutate(t = as.numeric(Variable) - 90,
         t = ifelse(t < 0, 10,t),
         YEAR = t + 1990) %>%
  arrange(CountyFIPS,t)

f.ctyDG90 <- inner_join(f.ctyYr90, f.ctyConst90, by="CountyFIPS") %>%
  rename(Q_t = "value") %>%
  mutate(DG_1 = P_0 + ((Q_t - P_0)*((P_10 - P_0)/(Q_10 - P_0))),
         DG_2 = Q_t + ((P_10 - Q_10)*t)/10,
         DG_3 = Q_t * ((((10-t)*Q_10) + (t * P_10))/(10 * Q_10)),
         DG_4 = (10 * P_10 * Q_t)/(((10 - t)* P_10) + (t * Q_10)),
         DG_6 = Q_t * ((P_10/Q_10)^(t/10))
  ) %>%
  select(CountyFIPS, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)

}

# 2000-2010
f.ctyDG00 <- indata00_10 %>%
  select(varSel00_10) %>%
  filter(CountyFIPS == ctyfips) %>%  filter(PlaceFIPS == "00000") %>%
  gather(HU, value, varSel00_10[3]:varSel00_10[14], factor_key=TRUE) %>%
  mutate(Variable = ifelse(HU == varSel00_10[14],"P_10", sub(prefix,"",HU)))

f.ctyConst00 <- f.ctyDG00 %>% filter(Variable %in% c("00","P_10","10")) %>%
  select(CountyFIPS, Variable, value)  %>%
  spread(Variable, value) %>% 
  rename(Q_10 = "10",P_0 = "00")

f.ctyYr00 <- f.ctyDG00 %>% filter(Variable != "P_10") %>%
  mutate(t = as.numeric(Variable),
         t = ifelse(t < 0, 10,t),
         YEAR = t + 2000) %>%
  arrange(CountyFIPS,t)

f.ctyDG00 <- inner_join(f.ctyYr00, f.ctyConst00, by="CountyFIPS") %>%
  rename(Q_t = "value") %>%
  mutate(DG_1 = P_0 + ((Q_t - P_0)*((P_10 - P_0)/(Q_10 - P_0))),
         DG_2 = Q_t + ((P_10 - Q_10)*t)/10,
         DG_3 = Q_t * ((((10-t)*Q_10) + (t * P_10))/(10 * Q_10)),
         DG_4 = (10 * P_10 * Q_t)/(((10 - t)* P_10) + (t * Q_10)),
         DG_6 = Q_t * ((P_10/Q_10)^(t/10)) ) %>%
  select(CountyFIPS, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)


# Combining files
if(ctyfips  != "014") {
  f.ctyDGFIN <- bind_rows(f.ctyDG90,f.ctyDG00[,2:12])
} else {
  f.ctyDGFIN <-f.ctyDG00
}
} else {  # Municipalities

  if(ctyfips != "014") { 
    f.PlTemp <- indata90_00 %>%
      filter(PlaceFIPS == plfips)
    
    if(nrow(f.PlTemp) > 1){
    if(type == "Persons Per Household") {
      varSel90_00pph <- c("CountyFIPS", "PlaceFIPS", "Hp90", "Ohu90", "Hp91", "Ohu91", "Hp92", "Ohu92",
                       "Hp93", "Ohu93", "Hp94", "Ohu94", "Hp95", "Ohu95", "Hp96", "Ohu96", "Hp97", "Ohu97",
                       "Hp98", "Ohu98", "Hp99", "Ohu99", "Hp00", "Ohu00", "Hp00x", "Ohu00x")
      f.PlSum <- f.PlTemp %>%
              select(varSel90_00pph) %>%
              group_by(PlaceFIPS) %>%
              summarize_at(vars(Hp90:Ohu00x), sum, na.rm = TRUE) %>%
              mutate(CountyFIPS = "999",
                     Pph90 = Hp90/Ohu90,
                     Pph91 = Hp91/Ohu91,
                     Pph92 = Hp92/Ohu92,
                     Pph93 = Hp93/Ohu93,
                     Pph94 = Hp94/Ohu94,
                     Pph95 = Hp95/Ohu95,
                     Pph96 = Hp96/Ohu96,
                     Pph97 = Hp97/Ohu97,
                     Pph98 = Hp98/Ohu98,
                     Pph99 = Hp99/Ohu99,
                     Pph00 = Hp00/Ohu00,
                     Pph00x = Hp00x/Ohu00x
                     ) %>%
             select(CountyFIPS, PlaceFIPS, Pph90:Pph00x)
    } else {
      f.PlSum <- f.PlTemp %>%
        select(varSel90_00) %>%
        group_by(PlaceFIPS) %>%
        summarize_at(vars(varSel90_00[3]:varSel90_00[14]), sum, na.rm = TRUE) %>%
        mutate(CountyFIPS = "999") %>%
        select(varSel90_00)
    }
    } else {
      f.PlSum <- f.PlTemp %>% select(varSel90_00)
    }
    
    
    f.ctyDG90 <-  f.PlSum %>%
      gather(HU, value, varSel90_00[3]:varSel90_00[14], factor_key=TRUE) %>%
      mutate(Variable = ifelse(HU == varSel90_00[14],"P_10", sub(prefix,"",HU)))
    
    
    f.ctyConst90 <- f.ctyDG90 %>% filter(Variable %in% c("90","P_10","00")) %>%
      select(CountyFIPS, Variable, value)  %>%
      spread(Variable, value) %>% 
      rename(Q_10 = "00",P_0 = "90")
    
    f.ctyYr90 <- f.ctyDG90 %>% filter(Variable != "P_10") %>%
      mutate(t = as.numeric(Variable) - 90,
             t = ifelse(t < 0, 10,t),
             YEAR = t + 1990) %>%
      arrange(CountyFIPS,t)
    
    f.ctyDG90 <- inner_join(f.ctyYr90, f.ctyConst90, by="CountyFIPS") %>%
      rename(Q_t = "value") %>%
      mutate(DG_1 = P_0 + ((Q_t - P_0)*((P_10 - P_0)/(Q_10 - P_0))),
             DG_2 = Q_t + ((P_10 - Q_10)*t)/10,
             DG_3 = Q_t * ((((10-t)*Q_10) + (t * P_10))/(10 * Q_10)),
             DG_4 = (10 * P_10 * Q_t)/(((10 - t)* P_10) + (t * Q_10)),
             DG_6 = Q_t * ((P_10/Q_10)^(t/10))) %>%
      select(CountyFIPS, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)
    
 
  }  # CountyFIPS != 014
  
  # 2000-2010

  f.PlTemp <- indata00_10 %>%
    select(varSel00_10) %>%
    filter(PlaceFIPS == plfips)
  
  if(nrow(f.PlTemp) > 1){
    f.PlSum <- f.PlTemp %>% filter(CountyFIPS == "999")
  } else {
    f.PlSum <- f.PlTemp
  }
  
  f.ctyDG00 <- f.PlSum %>%
    gather(HU, value, varSel00_10[3]:varSel00_10[14], factor_key=TRUE) %>%
    mutate(Variable = ifelse(HU == varSel00_10[14],"P_10", sub(prefix,"",HU)))
  
  f.ctyConst00 <- f.ctyDG00 %>% filter(Variable %in% c("00","P_10","10")) %>%
    select(CountyFIPS, Variable, value)  %>%
    spread(Variable, value) %>% 
    rename(Q_10 = "10",P_0 = "00")
  
  f.ctyYr00 <- f.ctyDG00 %>% filter(Variable != "P_10") %>%
    mutate(t = as.numeric(Variable),
           t = ifelse(t < 0, 10,t),
           YEAR = t + 2000) %>%
    arrange(CountyFIPS,t)
  
  f.ctyDG00 <- inner_join(f.ctyYr00, f.ctyConst00, by="CountyFIPS") %>%
    rename(Q_t = "value") %>%
    mutate(DG_1 = P_0 + ((Q_t - P_0)*((P_10 - P_0)/(Q_10 - P_0))),
           DG_2 = Q_t + ((P_10 - Q_10)*t)/10,
           DG_3 = Q_t * ((((10-t)*Q_10) + (t * P_10))/(10 * Q_10)),
           DG_4 = (10 * P_10 * Q_t)/(((10 - t)* P_10) + (t * Q_10)),
           DG_6 = Q_t * ((P_10/Q_10)^(t/10))) %>%
    select(CountyFIPS, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)
 
  
  # Combining files
  if(ctyfips  != "014") {
    f.ctyDGFIN <- bind_rows(f.ctyDG90,f.ctyDG00[,2:12])
  } else {
    f.ctyDGFIN <-f.ctyDG00
  }
}   # Municipalities

  

  f.ctyDGFIN <- as.data.frame(f.ctyDGFIN)
  
  if(type == "Persons Per Household") {
    f.ctyDGFIN$Q_t_txt  <- paste0(type,"<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$Q_t,digits=5))
    f.ctyDGFIN$DG_1_txt <- paste0("Das Gupta 1<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$DG_1,digits=5))
    f.ctyDGFIN$DG_2_txt <- paste0("Das Gupta 2<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$DG_2,digits=5))
    f.ctyDGFIN$DG_3_txt <- paste0("Das Gupta 3<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$DG_3,digits=5))
    f.ctyDGFIN$DG_4_txt <- paste0("Das Gupta 4<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$DG_4,digits=5))
    f.ctyDGFIN$DG_6_txt <- paste0("Das Gupta 6<br>",f.ctyDGFIN$YEAR," ",signif(f.ctyDGFIN$DG_6,digits=5))
  } else {
    f.ctyDGFIN$Q_t_txt  <- paste0(type,"<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$Q_t))
    f.ctyDGFIN$DG_1_txt <- paste0("Das Gupta 1<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$DG_1))
    f.ctyDGFIN$DG_2_txt <- paste0("Das Gupta 2<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$DG_2))
    f.ctyDGFIN$DG_3_txt <- paste0("Das Gupta 3<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$DG_3))
    f.ctyDGFIN$DG_4_txt <- paste0("Das Gupta 4<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$DG_4))
    f.ctyDGFIN$DG_6_txt <- paste0("Das Gupta 6<br>",f.ctyDGFIN$YEAR," ",NumFmt(f.ctyDGFIN$DG_6))
    }
   
# Creating Tick Ranges Year axis

minYear <- min(f.ctyDGFIN$YEAR)

valRange <- as.data.frame(rangeVal(f.ctyDGFIN$Q_t, f.ctyDGFIN$DG_1, f.ctyDGFIN$DG_2, f.ctyDGFIN$DG_3, f.ctyDGFIN$DG_4, f.ctyDGFIN$DG_6))

if(type == "Persons Per Household") {
  valRange$max <- plyr::round_any(valRange$max, 0.01, f = ceiling)
  valRange$range <- (valRange$max - valRange$min)/10
} else {
  valRange$min <- floor(valRange$min)
  if(valRange$max < 1000) {
     valRange$max <- plyr::round_any(valRange$max, 10, f = ceiling)
  } else if(valRange$max < 2500) {
    valRange$max <- plyr::round_any(valRange$max, 100, f = ceiling)
  } else {
    valRange$max <- plyr::round_any(valRange$max, 1000, f = ceiling)
  }
  valRange$range <- round((valRange$max - valRange$min)/10,digits=0)
}


# Titles
if(type == "Total Housing Units") {
  if(plfips == "") {
    total_tit <- paste0("Total Housing Units, ", ctyname)
  } else {
    total_tit <- paste0("Total Housing Units, ", plname)
  }
  seriesName <- "Total Housing Units"
}

if(type == "Population") {
  if(plfips == "") {
     total_tit <- paste0("Population, ",ctyname)
  } else {
    total_tit <- paste0("Population, ",plname)
  }
  seriesName <- "Population"
}

if(type == "Persons Per Household") {
  if(plfips == "") {
     total_tit <- paste0("Persons Per Household, ", ctyname)
  } else {
    total_tit <- paste0("Persons Per Household, ", plname)
  }
  seriesName <- "Persons Per Household"
}


lineCh <- plot_ly(width=1000, height=500, f.ctyDGFIN, x = ~YEAR, y = ~Q_t, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'rgb(0,0,0)'),
                  marker = list(color = 'rgb(0,0,0)'),
                  name = seriesName,text = ~Q_t_txt, hoverinfo = 'text')    %>% 
  config(
    toImageButtonOptions = list(
      format = "png",
      filename = total_tit,
      width = 1000, height = 500
    ))

if("DG_1" %in% DGList) {
  lineCh <- lineCh %>% add_trace(y = ~DG_1, type = 'scatter', mode = 'lines+markers',
                                 line = list(color = 'rgb(0,0,153)'),
                                 marker = list(color = 'rgb(0,0,153)'),
                                 name = "Das Gupta 1",text = ~DG_1_txt, hoverinfo = 'text')
}

if("DG_2" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_2, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(0,153,0)'),
                               marker = list(color = 'rgb(0,152,0)'),
                               name = "Das Gupta 2",text = ~DG_2_txt, hoverinfo = 'text')
}

if("DG_3" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_3, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(102,102,0)'),
                               marker = list(color = 'rgb(102,102,0)'),
                               name = "Das Gupta 3",text = ~DG_3_txt, hoverinfo = 'text')
}

if("DG_4" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_4, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(204,102,0)'),
                               marker = list(color = 'rgb(204,102,0)'),
                               name = "Das Gupta 4",text = ~DG_4_txt, hoverinfo = 'text')
}

if("DG_6" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_6, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(255,0,0)'),
                               marker = list(color = 'rgb(255,0,0)'),
                               name = "Das Gupta 6",text = ~DG_6_txt, hoverinfo = 'text')
}

if(type == "Persons Per Household") {
  lineCh <- lineCh %>% layout(margin = list(l = 50, r = 50, t = 75, b = 105),
                              title = total_tit,
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              hoverlabel = "right",
                              xaxis = list(title = "Year",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE,
                                           tick0 = minYear,
                                           dtick = 2),
                              yaxis = list(range = c(valRange$min, valRange$max),
                                           title = seriesName,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           showticklabels = TRUE,
                                           autotick = FALSE,
                                           ticks = 'outside',
                                           tickmode = 'linear',
                                           tick0 = valRange$min,
                                           dtick = valRange$range,
                                           zeroline = FALSE),
                              legend = list(legend = list(x = 100, y = 0.5)))
  } else {
  lineCh <- lineCh %>% layout(margin = list(l = 50, r = 50, t = 75, b = 105),
                              title = total_tit,
                              paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                              hoverlabel = "right",
                              xaxis = list(title = "Year",
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           ticks = 'outside',
                                           zeroline = FALSE,
                                           tick0 = minYear,
                                           dtick = 2),
                              yaxis = list(range = c(valRange$min, valRange$max),
                                           title = seriesName,
                                           gridcolor = 'rgb(255,255,255)',
                                           showgrid = TRUE,
                                           showline = FALSE,
                                           showticklabels = TRUE,
                                           tickcolor = 'rgb(127,127,127)',
                                           showticklabels = TRUE,
                                           autotick = FALSE,
                                           ticks = 'outside',
                                           tickmode = 'linear',
                                           tick0 = valRange$min,
                                           dtick = valRange$range,
                                           tickformat = ",d",
                                           zeroline = FALSE),
                              legend = list(legend = list(x = 100, y = 0.5)))
}



outlist <- list("PLOT" = lineCh, "DATA" = f.ctyDGFIN)
return(outlist)

} # End GenDG