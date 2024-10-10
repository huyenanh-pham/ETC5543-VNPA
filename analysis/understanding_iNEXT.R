## import packages 
library(iNEXT) 
library(ggplot2)
library(readr)

# -------------------------------------------------------
# default datasets from iNEXT

# iNEXT(x, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95, 
#       nboot=50)

# list of two vectors
data(spider)
str(spider) 
iNEXT(spider, q=0, datatype="abundance")

# How many occurences of each bird species per surveyed site
?iNEXT::bird
data(bird)
out <- iNEXT(bird, datatype="abundance")

# data collected from three coastal dune habitats: 
# $ EtoshaPan
# $ CentralNamibDesert
# $ SouthernNamibDesert
# a list of three species-by-plots matrices
data(ciliates) 
str(ciliates) 

# Get the 1st list of species-by-plots matrix = $EtoshaPan (1 out of 3 habitats)
ciliates[1]

out.raw <- iNEXT(ciliates, q = 0, datatype="incidence_raw", endpoint=150) 
out.raw
1
#
data(ant)
str(ant)
ant[1]


# -------------------------------------------------------
# [iNEXTOnline_UserGuide](https://drive.google.com/file/d/1GBObwqUWMOVGexczJP6Q4vY_e4SSY54W/view)

# For example, the classic rarefaction method involves 
# - rarefying all sample sizes to the minimum sample size, and 
# - then comparing the diversities for the minimum sample size. 

# The data include spider species abundances in two canopy manipulation treatments (Girdled and Logged) of hemlock trees. 
### In the Girdled treatment site, there were 26 species among 168 individuals; 
### in the Logged treatment site, there were 37 species among 252 individuals. 
data(spider)
spider 
# A list of 2 vectors
# $ Girdled: num [1:26] 46 22 17 15 15 9 8 6 6 4 ...
# $ Logged : num [1:37] 88 22 16 15 13 10 8 8 7 7 ...

# For the spider data, 
# the sample sizes for the Girdled sites are 168
# the sample sizes for the Logged sites are 252; 
# -> thus classic rarefaction is to down-sample the Logged data to a size of 168. The following commands return the corresponding diversities of three orders (q=0, 1 and 2) along with sample coverage (SC) for the size of 168:

spider_output <- estimateD(spider, datatype="abundance", base="size", level=NULL, conf=0.95)
spider_output

# FOREST PLOT 
spider_output |> 
  ggplot(aes(x=Assemblage, y=qD)) + 
  geom_point() + 
  ggplot2::geom_errorbar(aes(ymin = qD.LCL, ymax = qD.UCL)
                         #, linewidth = ebsize, width = ebw
                         ) +
  facet_wrap(~Order.q)

# The sample completeness of the reference samples for the Girdled and Logged sites are respectively 92.89% and 94.46%. As with classic rarefaction, we can also rarefy the Logged data to the lower coverage value; here we can only rarefy to the closet value of 92.90% due to the constraint that the sample size must be an integer.

iNEXT(spider, q=0, datatype="abundance")$DataInfo

# $AsyEst: asymptotic diversity estimates along with related statistics.
# Assemblage         Diversity  Observed Estimator       s.e.       LCL        UCL
# 1    Girdled  Species richness 26.000000 43.892857 20.3903863 26.000000  83.857280
# 2    Girdled Shannon diversity 12.059650 13.826253  1.3468978 11.186382  16.466125
# 3    Girdled Simpson diversity  7.840000  8.174825  0.9518326  6.309267  10.040383
# 4     Logged  Species richness 37.000000 61.402778 27.5094833 37.000000 115.320374
# 5     Logged Shannon diversity 14.421002 16.337069  1.6737124 13.056653  19.617485
# 6     Logged Simpson diversity  6.761499  6.920350  0.9244112  5.108537   8.732163

# ------------------------------------------------------- 

# # Get data 
# gorta <- read_csv(here::here("raw-data/Gorta_et_al_fires_data.csv"))
# 
# # gorta |> 
# #   dplyr::filter(group == "Birds") |> 
# #   group_by(group) |> summarise(n = n())
# 
# # ESTIMATING 
# gorta_output <- iNEXT::estimateD(
#   x = gorta[c("group", "sample")],
#   q = c(0, 1, 2),
#   datatype="abundance", # abundance data
#   base = "size",        # comparison base = sample-size-based
#   conf=0.95,
#   nboot=50)
# 
# gorta |>
#   dplyr::filter(before_after_fire  == "after",
#                 burn == "Burnt",
#                 group == "Birds")



