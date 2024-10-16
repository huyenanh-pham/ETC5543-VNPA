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


# FOREST PLOT 
spider_output |> 
  ggplot(aes(x=Assemblage, y=qD)) + 
  geom_point() + 
  ggplot2::geom_errorbar(aes(ymin = qD.LCL, ymax = qD.UCL)
                         #, linewidth = ebsize, width = ebw
  ) +
  facet_wrap(~Order.q)
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

gorta |> group_by(vegetation_grouping, burn) |> 
  summarise(gridcell_count = n_distinct(gridcell_id)) |> 
  pivot_wider(names_from = burn, values_from = gridcell_count) |> 
  mutate(gridcell_count_diff = Burnt - Unburnt)

gorta |> group_by(sample) |> 
  summarise(gridcell_count = n_distinct(gridcell_id)) |> 
  View()



# -------------------------------------------------------
# iNEXT datatype="incidence_raw"
data(ciliates)
ciliates

# iNEXT datatype="incidence_freq"
data(ant)
ant

# data(ant) contains 5 lists of incidence frequencies = Ant species incidence frequencies for samples from five elevations/assemblages in northeastern Costa Rica (Longino and Colwell 2011). 
# The number of sampling units (1m x 1m forest floor plot) for the 5 assemblages are respectively 599, 230, 150, 200 and 200. 
# The number of observed species for the 5 assemblages are respectively 227, 241, 122, 56 and 14.
# number of observed species in each 

# the species diversity with a specified level of sample coverage of 98.5% for the ant data
estimateD(ant, datatype="incidence_freq",  
          base="coverage", level=0.985, conf=0.95)
# Assemblage        t        Method Order.q    SC         qD     qD.LCL     qD.UCL
# 1        h50m 327.1646   Rarefaction       0 0.985 197.487977 183.861496 211.114458
# 2        h50m 327.1646   Rarefaction       1 0.985  78.052670  75.874634  80.230705
# 3        h50m 327.1646   Rarefaction       2 0.985  50.461029  48.760417  52.161640
# 4       h500m 342.8592 Extrapolation       0 0.985 268.725933 239.980363 297.471502
# 5       h500m 342.8592 Extrapolation       1 0.985 103.847150  99.937824 107.756476
# 6       h500m 342.8592 Extrapolation       2 0.985  64.758264  62.511803  67.004725
# 7      h1070m 158.9508 Extrapolation       0 0.985 123.608792 109.574660 137.642923
# 8      h1070m 158.9508 Extrapolation       1 0.985  59.591818  57.063410  62.120227
# 9      h1070m 158.9508 Extrapolation       2 0.985  41.775173  39.604919  43.945428
# 10     h1500m 125.9590   Rarefaction       0 0.985  50.478877  42.979249  57.978505
# 11     h1500m 125.9590   Rarefaction       1 0.985  26.248998  24.685904  27.812092
# 12     h1500m 125.9590   Rarefaction       2 0.985  18.648902  17.451486  19.846318
# 13     h2000m 104.6306   Rarefaction       0 0.985  12.909623  11.323765  14.495481
# 14     h2000m 104.6306   Rarefaction       1 0.985   7.710717   6.860985   8.560450
# 15     h2000m 104.6306   Rarefaction       2 0.985   5.794580   5.015308   6.573851


# Paper Source / ref
# Chao, A., Gotelli, N. J., Hsieh, T. C., Sander, E. L., Ma, K. H., Colwell, R.  K., & Ellison, A. M. (2014). Rarefaction and extrapolation with hill numbers: A framework for sampling and estimation in species diversity studies. Ecological Monographs, 84(1), 45–67. (https://doi.org/10.1890/13-0133.1)

# iNEXT: Interpolation and extrapolation of Hill number with order q
# iNEXT::iNEXT(ant, datatype = "incidence_freq") <start> -----------------------

# Compare 5 assemblages with Hill number order q = 0.
# $class: iNEXT

# $DataInfo: basic data information
# Assemblage   T    U S.obs     SC Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10
# 1       h50m 599 5976   227 0.9918 49 23 18 14  9 10  4  8  6   2
# 2      h500m 230 2943   241 0.9760 71 34 12 14  9 11  8  4  7   5
# 3     h1070m 150 1730   122 0.9839 28 16 13  3  1  3  6  1  1   1
# 4     h1500m 200 1170    56 0.9889 13  4  2  2  4  2  0  0  4   0
# 5     h2000m 200  271    14 0.9964  1  2  1  1  0  0  0  2  0   0

# $iNextEst: diversity estimates with rarefied and extrapolated samples.
# $size_based (LCL and UCL are obtained for fixed size.)

# Assemblage    t        Method Order.q         qD     qD.LCL     qD.UCL        SC    SC.LCL    SC.UCL
# 1         h50m    1   Rarefaction       0   9.976628   9.742523  10.210732 0.1952498 0.1879846 0.2025150
# 10        h50m  299   Rarefaction       0 193.069392 186.386235 199.752548 0.9835584 0.9820267 0.9850900
# 20        h50m  599      Observed       0 227.000000 217.775540 236.224460 0.9918134 0.9899299 0.9936969
# 30        h50m  883 Extrapolation       0 245.732481 232.633653 258.831309 0.9947564 0.9926224 0.9968904
# 40        h50m 1198 Extrapolation       0 258.745498 240.551022 276.939974 0.9968008 0.9949011 0.9987005
# 41       h500m    1   Rarefaction       0  12.795652  12.415958  13.175347 0.1952438 0.1847625 0.2057252
# 50       h500m  115   Rarefaction       0 194.186777 185.748962 202.624593 0.9570103 0.9524236 0.9615970
# 60       h500m  230      Observed       0 241.000000 228.418974 253.581026 0.9759754 0.9714516 0.9804993
# 70       h500m  339 Extrapolation       0 267.977661 251.399435 284.555886 0.9847564 0.9802190 0.9892939
# 80       h500m  460 Extrapolation       0 286.546519 265.259424 307.833613 0.9908005 0.9868886 0.9947123
# 81      h1070m    1   Rarefaction       0  11.533333  11.023070  12.043597 0.2714978 0.2570016 0.2859941
# 90      h1070m   75   Rarefaction       0 102.135118  96.755503 107.514734 0.9674024 0.9620625 0.9727424
# 100     h1070m  150      Observed       0 122.000000 113.916340 130.083660 0.9839382 0.9791893 0.9886871
# 110     h1070m  221 Extrapolation       0 132.189999 121.893357 142.486641 0.9906634 0.9853313 0.9959956
# 120     h1070m  300 Extrapolation       0 138.600961 125.279293 151.922628 0.9948946 0.9900011 0.9997880
# 121     h1500m    1   Rarefaction       0   5.850000   5.586788   6.113212 0.3081991 0.2894638 0.3269344
# 130     h1500m  100   Rarefaction       0  47.982643  44.376518  51.588768 0.9819701 0.9772630 0.9866772
# 140     h1500m  200      Observed       0  56.000000  50.957604  61.042396 0.9889231 0.9849963 0.9928500
# 150     h1500m  295 Extrapolation       0  61.343524  55.448228  67.238820 0.9917391 0.9872376 0.9962406
# 160     h1500m  400 Extrapolation       0  65.684073  58.447469  72.920676 0.9940265 0.9894064 0.9986465
# 161     h2000m    1   Rarefaction       0   1.355000   1.217526   1.492474 0.2264459 0.1944342 0.2584575
# 170     h2000m  100   Rarefaction       0  12.811923  11.421512  14.202335 0.9840431 0.9756225 0.9924637
# 180     h2000m  200      Observed       0  14.000000  12.157983  15.842017 0.9963827 0.9885206 1.0000000
# 190     h2000m  295 Extrapolation       0  14.211194  11.957533  16.464854 0.9994539 0.9942811 1.0000000
# 200     h2000m  400 Extrapolation       0  14.244103  11.631019  16.857187 0.9999324 0.9963553 1.0000000

# NOTE: The above output only shows five estimates for each assemblage; call iNEXT.object$iNextEst$size_based to view complete output.

# $coverage_based (LCL and UCL are obtained for fixed coverage; interval length is wider due to varying size in bootstraps.)

# Assemblage        SC    t        Method Order.q         qD     qD.LCL     qD.UCL
# 1         h50m 0.1952506    1   Rarefaction       0   9.976668   9.764831  10.188505
# 10        h50m 0.9835584  299   Rarefaction       0 193.069392 183.803012 202.335773
# 20        h50m 0.9918134  599      Observed       0 227.000000 211.631902 242.368098
# 30        h50m 0.9947564  883 Extrapolation       0 245.732481 223.678947 267.786015
# 40        h50m 0.9968008 1198 Extrapolation       0 258.745498 231.337069 286.153928
# 41       h500m 0.1952438    1   Rarefaction       0  12.795652  12.399490  13.191815
# 50       h500m 0.9570103  115   Rarefaction       0 194.186787 180.462192 207.911382
# 60       h500m 0.9759754  230      Observed       0 241.000000 220.809447 261.190553
# 70       h500m 0.9847564  339 Extrapolation       0 267.977661 242.743185 293.212136
# 80       h500m 0.9908005  460 Extrapolation       0 286.546519 257.083336 316.009702
# 81      h1070m 0.2714996    1   Rarefaction       0  11.533405  11.023056  12.043754
# 90      h1070m 0.9674024   75   Rarefaction       0 102.135111  93.440454 110.829769
# 100     h1070m 0.9839382  150      Observed       0 122.000000 107.469867 136.530133
# 110     h1070m 0.9906634  221 Extrapolation       0 132.189999 112.939179 151.440819
# 120     h1070m 0.9948946  300 Extrapolation       0 138.600961 115.278557 161.923364
# 121     h1500m 0.3081991    1   Rarefaction       0   5.850000   5.575728   6.124272
# 130     h1500m 0.9819701  100   Rarefaction       0  47.982646  41.978576  53.986715
# 140     h1500m 0.9889231  200      Observed       0  56.000000  45.315043  66.684957
# 150     h1500m 0.9917391  295 Extrapolation       0  61.343524  48.038943  74.648105
# 160     h1500m 0.9940265  400 Extrapolation       0  65.684073  50.404453  80.963693
# 161     h2000m 0.2264480    1   Rarefaction       0   1.355012   1.227979   1.482045
# 170     h2000m 0.9840431  100   Rarefaction       0  12.811923  10.883818  14.740028
# 180     h2000m 0.9963827  200      Observed       0  14.000000  11.033772  16.966228
# 190     h2000m 0.9994539  295 Extrapolation       0  14.211194  10.736113  17.686275
# 200     h2000m 0.9999324  400 Extrapolation       0  14.244103  10.680815  17.807391

# NOTE: The above output only shows five estimates for each assemblage; call iNEXT.object$iNextEst$coverage_based to view complete output.

# $AsyEst: asymptotic diversity estimates along with related statistics.
# Assemblage         Diversity   Observed  Estimator       s.e.        LCL        UCL
# 1      h1070m  Species richness 122.000000 146.336667 12.4978039 122.000000 170.831912
# 2      h1070m Shannon diversity  59.446301  62.043936  1.4307595  59.239699  64.848173
# 3      h1070m Simpson diversity  41.733832  42.480379  1.1865892  40.154707  44.806052
# 4      h1500m  Species richness  56.000000  77.019375 18.4919413  56.000000 113.262914
# 5      h1500m Shannon diversity  26.656913  27.516175  0.7527388  26.040834  28.991515
# 6      h1500m Simpson diversity  18.770568  18.981237  0.5563731  17.890766  20.071708
# 7      h2000m  Species richness  14.000000  14.248750  2.0952048  14.000000  18.355276
# 8      h2000m Shannon diversity   7.903231   8.103454  0.4753716   7.171743   9.035165
# 9      h2000m Simpson diversity   5.883281   5.983769  0.4354085   5.130384   6.837154
# 10      h500m  Species richness 241.000000 314.810038 17.6766597 280.164422 349.455655
# 11      h500m Shannon diversity 101.705574 107.601543  2.0641419 103.555899 111.647186
# 12      h500m Simpson diversity  64.382979  65.536774  1.5579091  62.483328  68.590220
# 13       h50m  Species richness 227.000000 279.108514 19.6931997 240.510552 317.706476
# 14       h50m Shannon diversity  79.485435  81.440350  1.1251677  79.235062  83.645638
# 15       h50m Simpson diversity  50.747554  51.096751  0.8166395  49.496167  52.697335

# iNEXT::iNEXT(ant, datatype = "incidence_freq") <end> -------------------------
ant_iNEXT <- iNEXT::iNEXT(ant, q=0, datatype = "incidence_freq")
ant_iNEXT$DataInfo

estimateD(ant, datatype="incidence_freq", base="coverage", level=NULL, conf=0.95)

# UNDERSTANDING THE incidence_freq DATA FORMAT ---------------------------------
# A list of vectors
# each vector = a evc_group
#   (1) The first entry of each vector must be the total number of sampling units (T), 
#   (2) followed by the species incidence frequencies
#   Length of each vector = Sobs = length(ant[[1]]) - 1 = 227 = number of observed species for the 1st evc_group

# (1) T (NUMBER OF SAMPLING UNITS) = 599 = number of grid cells -> (How many grid cells were sampled in each evc_group?)
# (2) SPECIES INCIDENCE FREQUENCIES -> the number of sampling units in which species i was detected
# ------------------------------------------------------------------------------

sighting_final$species_group |> unique()
# "Amphibians" "Plants"     "Birds"      "Molluscs"   "Insects"    "Mammals"    "Reptiles"