###Install the Terra package
##Methods for spatial data analysis with raster and vector data.
##More info on terra https://cran.r-project.org/web/packages/terra/index.html
install.packages("terra")
##Load the package in the Rstudio session
library(rasterVis)
library(terra)
library(zoo)
###Create the path for the Chirps dataset
##This is the directory in your pc where Chirps is stored
##In my case is
path <- "E:/Thao/chirps_monthly"
##Read the names of the files and create a list() from them
Chirps_files <- list.files(path,
                           pattern = NULL,
                           all.files = FALSE,
                           full.names = TRUE)
##Create a stack, more info https://r-spatial.github.io/stars/
chirps <- rast(Chirps_files)
##A raster stack has 3 dimensions (X,Y,Time), each of the rasters contains the monthly average precipitation  
##each of the pixels in the rasters contain the precipitation value on the location for that month
##If we want to plot the data because the stack has 3 dimentions we use variablename[[]] to access a specific time
plot(chirps[[1]])
##Since Chirps is a global product we need to crop the area to our study region
##For doing that, we are going to use a Shapefile as a "cookie cutter" to extract only the information inside our area
##In this example, I'm going to extract the data for the Rapel Basin in Chile 
##to load a shapefile we use the vect function from the Terra package.
## IS KEY TO ONLY USE THE SHP FILE 
## Avoid loading the files ending in [SHX,XLXS...]
Rapel_basin_path <- "E:/Basin_Rapel/Cuenca_Rapel.shp"
Rapel_basin <- vect(Rapel_basin_path)
##Now we need to check that the raster stack and the shape file are in the same georeference system
##Otherwise the extends aren't going to match in the cropping process 
##For this we are using the function crs (coordinate reference system)
crs(basintest)
crs(basintest)
##If both are in the same coordinate system we can start to cut our study are
##We are going to use 2 functions for it
##The first one is crop() more info on https://rdrr.io/cran/terra/man/crop.html
chirps_crop <- crop(chirps, Rapel_basin, snap = 'out')
##plot the data
plot(chirps_crop[[1]])
##Show the outline of the basin
lines(Rapel_basin)
##The second one is the mask() function, this one eliminates any NA resulting from the crop
##ALWAYS CROP FIRST AND MASK SECOND, otherwise you will try to mask the whole extend of the datasheet
##And it takes a lot of processing and time
chirps_rapel <- mask(chirps_crop, Rapel_basin)
plot(chirps_rapel[[1]])
lines(Rapel_basin)
##Tapp() grabs the cropped raster stack and average for the index time period 
##because we are trying to get the monthly average we need to use an index=1:12
monthly_chirps<-tapp(chirps_rapel, index = 1:12, fun = mean)
##Creates and replace the names of the layers (X1,X2,X3) with the months  
names(monthly_chirps)<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
##Next we are going to make a levelplot (info https://www.rdocumentation.org/packages/lattice/versions/0.10-10/topics/levelplot) 
##for the monthly averages
levelplot( monthly_chirps,layout=c(4,3),contour=T)
##Extract mean precipitation
##This takes the already cropped and masked stack, and extracts the values inside a specific shapefile
extracted_values <- extract(chirps_rapel, Rapel_basin, fun = "mean")
##We transform the list to a dataframe and then transpose that so we can get a date/value format
##We avoid the first line that corresponds to the name of the rows
extracted_values <- as.numeric(extracted_values[2:481])
values <- as.data.frame(extracted_values)
##Now we can create a vector from the starting month to the last one
##From the file basenames ex "chirps-v2.0.2020.09.tif" we take from 2020 to 09 and use that to create a vector
Dates <- substr(basename(Chirps_files), 13, 19)
##The next step is to attach the dates and the extracted_values in a table 
Precipitation_Rapel <- cbind(Dates, values)
##Another method is creating a point layer and extract the locations from there 
##First step is to create the points with the respective location 

xy <- cbind(c(-71.445,-70.781,-70.358),c(-34.136,-34.487,-34.766))
p <- vect(xy, crs="+proj=longlat +datum=WGS84",atts=locations)
plot(Rapel_basin)
points(p)
##Extract the values for the point layer 
##The extract function can also use other functions (max,min,sum)
extracted_values_points <- extract(chirps_rapel, p, fun = "mean")
##We need to crop the index row so we eliminate the first one from the data frame
values_points <- (as.data.frame(extracted_values_points[2:481]))
Dates <- substr(basename(Chirps_files), 13, 19)
##Now we transpose the rows with the columns to create a table and then bind the rows with the Dates vector   
Precipitation_Rapel<-t(rbind(Dates,values_points))
Precipitation_Rapel<-as.data.frame(Precipitation_Rapel)
##Next we assign the headers for our table 
colnames(Precipitation_Rapel)<-c('date','lower','middle','upper')
##and saving it as a CSV file with write.csv()
##For saving files please remember to use the file extension of the format you want to save in 
##For this case a CSV
write.csv(Precipitation_Rapel,"E:/Basin_Rapel/Cuenca_Rapel4.csv")


