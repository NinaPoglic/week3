library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## TASK 1 - Segmentation
caro <-read_delim("caro60.txt",",")

## Calculating Euclidean distances between points within the temporal window

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),  #distance to pos -3
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),  #distance to pos -2
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),  #distance to pos -1
    nPlus1 = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), #distance to pos +1
    nPlus2 = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), #distance to pos +2
    nPlus3 = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  #distance to pos +3
  )

## TASK 2 - Specify and apply threshold d

## Mean distance for each row
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1, nPlus1, nPlus2, nPlus3))
  ) %>%
  ungroup()

## stepMean
summary(caro)

## steps & moves
caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

## TASK 3 - Visualize segmented trajectories
caro %>% 
  ggplot(aes(y=N, x=E) ) +
  geom_path()+ geom_point(aes(colour=static)) + coord_equal() 

## TASK 4 - Segment-based analysis
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}
caro <- caro %>%
  mutate(segment_id = rle_id(static))
## Plot
caro %>%
  group_by(segment_id) %>%
  ggplot(aes(y=N, x=E,col=segment_id ) ) +
  geom_path()+ geom_point() + coord_equal()

## Duration of segments & remove < 5 
caro <- caro %>%
  group_by(segment_id) %>%
  mutate(
    segm_duration = as.integer(difftime(max(DatetimeUTC),min(DatetimeUTC), units="mins"))
  ) %>% 
  filter(segm_duration > 5)  
## Plot
caro  %>%
  ggplot(aes(y=N, x=E,col=segment_id)) +
  geom_path()+ geom_point() + coord_equal()

## TAKS 5 - Similarity measures
pedestrian <- read_delim("pedestrian.txt",",") 

str(pedestrian)
pedestrian$TrajID=as.factor(pedestrian$TrajID)

pedestrian %>%
  ggplot(aes(y=N, x=E)) +
  geom_path()+ geom_point(aes(col=TrajID)) + 
  coord_equal() + 
  facet_wrap(~TrajID,nrow=2) +
  labs(title="Visual comparison of the 6 trajectories", subtitle="Each subplot highlights a trajectory")


library(SimilarityMeasures)
library(purrr)
library(tidyr)
library(ggpubr)
## TASK 6 - Calculate similarity
pedestrians_matrix <- pedestrian %>%
  dplyr::select(E, N) %>%            
  split(pedestrian$TrajID) %>%   # different trajectories
  purrr::map(as.matrix) 
?map
View(pedestrians_matrix)

## DTW
dtw_fun= function(y) {
  dtw = DTW(pedestrians_matrix[[1]],y)
  return(dtw)
}

dtw_all = pedestrians_matrix %>%
  map(dtw_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame()
dtw_all$trajectory=as.factor(dtw_all$trajectory)
dtw_plot=ggplot(dtw_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="Identity")
dtw_plot

## EditDist
editdist_fun= function(y) {
  Editdist = EditDist(pedestrians_matrix[[1]],y)
  return(Editdist)
}
editdist_all = pedestrians_matrix %>%
  map(editdist_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 
editdist_all$trajectory=as.factor(editdist_all$trajectory)
editdist_plot=ggplot(editdist_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="Identity")
editdist_plot

## Frechet
frechet_fun= function(y) {
  frechet = Frechet(pedestrians_matrix[[1]],y)
  return(frechet)
}
frechet_all = pedestrians_matrix %>%
  map(frechet_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 
frechet_all$trajectory=as.factor(frechet_all$trajectory)
frechet_plot=ggplot(frechet_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="Identity")

## LCSS
lcss_fun= function(y) {
  lcss = LCSS(pedestrians_matrix[[1]],y, pointSpacing=0.5, pointDistance=10,
              errorMarg=2, returnTrans=FALSE)
  return(lcss)
}
lcss_all = pedestrians_matrix %>%
  map(lcss_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 
lcss_all$trajectory=as.factor(lcss_all$trajectory)
lcss_plot=ggplot(lcss_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="Identity")

final_plot=ggarrange(dtw_plot, editdist_plot, frechet_plot,lcss_plot, ncol=2, nrow=2, common.legend=TRUE, legend = "right")
annotate_figure(final_plot,
                top="Computed similarities using different measures between trajectory 1 and all other trajectories")

