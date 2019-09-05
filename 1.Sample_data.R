
#####################################
## Earth Lab, Twensday
## Contact info: megan.cattau@colorado.edu
# This code: Imports shapefile data (e.g., hazards) and converts data to a list of annual rasters matching a uniform grid

# Required data: 
# EmptyGrid.tif to serve as a template for sampling
# Data you'd like to sample with attribute "Year" in the dataset

# Required packages:
library(raster)
library(rgdal)

setwd("/Users/megancattau 1/Dropbox/0_EarthLab/Twensday")


#####################################
## Import the data

# Empty sampling grid
template<-raster("EmptyGrid/Empty_250_US.tif")

# MTBS Fire data - WGS 1984 UTM Zone 13N
# Shown below with MTBS fire data as an example, but fire data can be replaced with any shapefile dataset
MTBS<-readOGR("MTBS","MTBS_WGS84_13N") 	



#####################################
## Parse the data by year and convert to a list of rasters

parsed_rasters <- function(all_data, prefix) {
	# The function takes a vector layer (arg: all_data; e.g., MTBS), parses it by year (requires attribute "Year" in the dataset), creates annual rasters of event count, and gives each new object a name starting with a defined prefix (arg: prefix; e.g., "fire") and ending with the year "_yyyy"
	# args= all_data (the original vector data), prefix (prefix for the name of the parsed raster layers)
	# returns a list of raster objects (count of events per pixel in a given year)

	# Give each event a unique ID
	all_data$uniqueID<-1:length(all_data)
	all_data_proj<-spTransform(all_data, crs(template))

	# Parse the data based on Year
	separate_data_by_year<-function(year){
		all_data_proj[all_data_proj@data$Year==year,]
	}
	
	# Apply this function over the range of years, resulting in a list of vector objects for each year
	vector_list <- lapply(min(all_data$Year):max(all_data$Year), separate_data_by_year)

	# Rasterize
	raster_each_year<- function(polygon){
	# Set the resolution and extent based on template raster
	r <- raster(ncol=ncol(template), nrow=nrow(template))
	extent(r)<-extent(template)
	projection(r)<-projection(template)
		
	if (length(polygon)==0)
		new_raster<-setValues(r, 0) 
	else
		new_raster<-rasterize(polygon, r, field="uniqueID", fun="count", background=0) # could change field, function, and background value here if you wanted to capture a different attribute than counts of events
		new_raster[is.na(new_raster)]<-0  # can remove this line if you want NA values rather than 0 if there is no hazard occurrence 
	new_raster
	}

	# Apply this function over the list of parsed vectors, resulting in a list of raster objects for each year
	annual_rasters<-lapply(vector_list, raster_each_year)
	names(annual_rasters) <- paste(prefix, min(all_data$Year):max(all_data$Year), sep = "_")

annual_rasters
}

fires<-parsed_rasters(MTBS, "fire") 




