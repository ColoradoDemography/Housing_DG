#' GenDG Calculates the Das Gupta estimates and persons per household 
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

  
# Reading County Housing Units data, with one year projection and Census estimate
 
# 1990-2000
f.ctyDG90 <- read_excel("data/County Housing Units.xlsx", sheet=1) %>%
  select(CountyFIPS:Thu00x) %>%
  filter(CountyFIPS == ctyfips) %>%
  gather(HU, value, Thu90:Thu00x, factor_key=TRUE) %>%
  mutate(Variable = ifelse(HU == "Thu00x","P_10", sub("Thu","",HU)))

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
  select(CountyFIPS, Area, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)

# 2000-2010
f.ctyDG00 <- read_excel("data/County Housing Units.xlsx", sheet=2) %>%
  select(CountyFIPS:Thu10x) %>%
  filter(CountyFIPS == ctyfips) %>%
  gather(HU, value, Thu00:Thu10x, factor_key=TRUE) %>%
  mutate(Variable = ifelse(HU == "Thu10x","P_10", sub("Thu","",HU)))

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
         DG_6 = Q_t * ((P_10/Q_10)^(t/10))
  ) %>%
  select(CountyFIPS, Area, t, YEAR, Q_t,Q_10, P_0, P_10, DG_1:DG_6)

# Combining files

f.ctyDGFIN <- bind_rows(f.ctyDG90,f.ctyDG00[2:11,])


# Titles
if(type == "Housing Units") {
  total_tit <- paste0("Total Housing Units, ", ctyname)
  seriesName <- "Total Housing Units"
}

if(type == "Population") {
  total_tit <- paste0("Population,",ctyname)
  seriesName <- "Population"
}

if(type == "Persons Per household") {
  total_tit <- paste0("Persons Per Household, ", ctyname)
  seriesName <- "Persons Per Household"
}


lineCh <- plot_ly(f.ctyDGFIN, x = ~YEAR, y = ~Q_t, type = 'scatter', mode = 'lines+markers',
                  line = list(color = 'rgb(0,0,0)'),
                  marker = list(color = 'rgb(0,0,0)'),
                  name = seriesName)    %>% 
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
                                 name = "Das Gupta 1")
}

if("DG_2" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_2, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(0,153,0)'),
                               marker = list(color = 'rgb(0,152,0)'),
                               name = "Das Gupta 2")
}

if("DG_3" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_3, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(102,102,0)'),
                               marker = list(color = 'rgb(102,102,0)'),
                               name = "Das Gupta 3")
}

if("DG_4" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_4, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(204,102,0)'),
                               marker = list(color = 'rgb(204,102,0)'),
                               name = "Das Gupta 4")
}

if("DG_6" %in% DGList) {
lineCh <- lineCh %>% add_trace(y = ~DG_6, type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'rgb(255,0,0)'),
                               marker = list(color = 'rgb(255,0,0)'),
                               name = "Das Gupta 6")
}

lineCh <- lineCh %>% layout(autosize = T,
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
                                         zeroline = FALSE),
                            yaxis = list(title = "Housing Units",
                                         gridcolor = 'rgb(255,255,255)',
                                         showgrid = TRUE,
                                         showline = FALSE,
                                         showticklabels = TRUE,
                                         tickcolor = 'rgb(127,127,127)',
                                         ticks = 'outside',
                                         zeroline = FALSE),
                            legend = list(legend = list(x = 100, y = 0.5)))


outlist <- list("PLOT" = lineCh, "DATA" = f.ctyDGFIN)
return(outlist)

} # End GenDG