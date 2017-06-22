

#------ PATHS ----####
#INPUTS  PATHS 
PA17path =      "/Users/henrygomory/Downloads/property-assessment-fy2017.csv"
P17shp_path =   "/Users/henrygomory/Downloads/Parcels2017DataFull/"  # from boston open data
P17shp_name =   "Parcels2017DataFull"  # from boston open data
pal16path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Property Assessment 2017/PADLong.Record.2016.csv"
malpath =       "/Users/henrygomory/Downloads/master-address-list.csv"
blocksShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Blocks/"
blocksShpName = "Blocks_Boston_2010_BARI"
# roads used for geocoding
roadsCSVPath  = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/roads_updated.csv"
roadsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/"
roadsShpName = "roads_updated"

#OUTPUTS PATHS
propertiesPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/Properties.2017.csv"
landParcelsShpPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.shp/"
landParcelsShpName = "LandParcels.2017"
LandParcelsCSVpath =      "Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
IDConnectorPath = "Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/IDConnector.2017.csv"

# READ IN FILES
PA17  = read.csv(PA17path, stringsAsFactors=F)
P17shp = readOGR(P17shp_path,P17shp_name,stringsAsFactors=F)
mal = read.csv(malpath,stringsAsFactors=F)
blocksShp = readOGR(blocksShpPath,blocksShpName,stringsAsFactors=F)
pal16 = read.csv(pal16path, stringsAsFactors=F)


#-------CLEANING--------####

# fix IDs
# this makes sure the names are standardized (PID -> parcel_num) and cleans the values
# these are in the cleaning functions file
PA17 = standardizeGeoNames(PA17)
P17shp@data = standardizeGeoNames(P17shp@data)


#sometimes the street numbers get interpreted as dates, this fixes that
months = c("-Jan","-Feb","-Mar","-Apr","-May","-Jun","-Jul","-Aug","-Sep","-Oct","-Nov","-Dec")
months2 = c("Jan-","Feb-","Mar-","Apr-","May-","Jun-","Jul-","Aug-","Sep-","Oct-","Nov-","Dec-")

for (i in c(1:12)) {
  PA17$ST_NUM[ grepl(months[i],PA17$ST_NUM)] =paste(i,gsub(months[i],"",PA17$ST_NUM[ grepl(months[i],PA17$ST_NUM)]),sep="-")
  PA17$ST_NUM = gsub(months2[i],paste(i,"-",sep=""),PA17$ST_NUM)
  
}

# cleans the addresses and splits them into multiple variables, to then be used when making the walkover datafile
# this should be combined with the identical cleaning of the PA17shp file (trivial)
# i put the street and suffix together and then split them apart using clean_address rather than call clean_street and clean_suffix individually because this way 
# it deals correctly with streets like "CHARLES STREET SOUTH"
temp = clean_address(paste(PA17$ST_NAME,PA17$ST_NAME_SUF,sep=" "))
PA17$street_c = temp[,4]
PA17$suffix_c = temp[,5]
PA17$directional_c = temp[,7]
rm(temp)
temp = clean_num(PA17$ST_NUM)
PA17$num1 = temp[,2]
PA17$num2 = temp[,3]
PA17$zip_c = clean_zip(PA17$ZIPCODE)
PA17$city_c = NA


#-------ADDING CENTROIDS--------####

#add centroid data to P17
P17shp.centroid = gCentroid(P17shp,byid=T)
P17shp@data$X = P17shp.centroid@coords[,1]
P17shp@data$Y = P17shp.centroid@coords[,2]


# add parcel centroids to TA 17
PA17 = merge(PA17,P17shp@data[!duplicated(P17shp@data$GIS_ID),c("GIS_ID","X","Y")],by="GIS_ID",all.x=T)
sum(!PA17$GIS_ID %in% P17shp$GIS_ID)
sum(is.na(PA17$X))

#--------GETTING CENSUS DATA through spatial overlay of blocks-------####
# we do a spatial overlay onto a census blocks shapefile, using the centroid-point geographical information from the parcels shapefile 
# we do a points over polygon overlay - it could have been the full parcels polygons, but this seemed safer
# there's confusing nomenclature here, PA17shp is the points version of PA17, with 170k or so rows, P17shp is the 90k rows polygons file

# get only the cases with geog data
PA17shp = PA17[!is.na(PA17$X),]
# make new coordinates that will disappear when we make a shapefile, so the shapefile will still have X and Y as variables
# that will not change values with different projections
PA17shp$Xcoord = PA17shp$X
PA17shp$Ycoord = PA17shp$Y
# make spatial points object
coordinates(PA17shp) = ~Xcoord+Ycoord
# set projection based on parcel shapefile
proj4string(PA17shp) = CRS(proj4string(P17shp))
# change projection to the blocks one
PA17shp <- spTransform(PA17shp, proj4string(blocksShp)) 
# do overlay to get block containing each point and bring the data over
# might want to add something that gets nearest block in case the point falls outside all blocks
# code for that can be found below in the part connecting the master address list and old property assessment longitudinal to the new GI
PA17shp.over = over(PA17shp,blocksShp)
for (var in c("Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  PA17shp@data[,var] = PA17shp.over[,var]
} 
# check how many we found
sum(is.na(PA17shp$Blk_ID_10))
# merge the data onto the original file
PA17 = merge(PA17, PA17shp@data[!duplicated(PA17shp@data$parcel_num), c("parcel_num","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],by="parcel_num",all.x=T)


#--------GETTING TLID through geocoding-------####
# trying to locate the closest road using just the XY was ineffective, often matching to close streets that had the wrong name
# geocoding based on street address also gave bad matches, because the street numbers in the roads file are estimates
# instead we geocode using only street name and suffix, then find the street that is geographically closest, getting the best of both worlds

# FOR NEXT TIME - it would probably be better to attach the polygons from P17shp to the PA17shp file, rather than using points, because that will be more accurate for the gDistance function in the geocoder

#only the non-duplicates are included (since the geocoder needs a unique ID and the files that duplicate parcel_num have the same geography)
PA17shp.nd = PA17shp[ !duplicated(PA17shp@data$parcel_num),]

# cleaning the shapefile to be geocoded
# i put them together and then split them apart using clean_address rather than call clean_street and clean_suffix individually because this way 
# it deals correctly with streets like "CHARLES STREET SOUTH"
temp = clean_address(paste(PA17shp.nd@data$ST_NAME,PA17shp.nd@data$ST_NAME_SUF,sep=" "))
PA17shp.nd@data$street_c = temp[,4]
PA17shp.nd@data$suffix_c = temp[,5]
PA17shp.nd@data$directional_c = temp[,7]
rm(temp)
temp = clean_num(PA17shp.nd@data$ST_NUM)
PA17shp.nd@data$num1 = temp[,2]
PA17shp.nd@data$num2 = temp[,3]
PA17shp.nd@data$zip_c = clean_zip(PA17shp.nd@data$ZIPCODE)
PA17shp.nd@data$city_c = NA


# geocoding in batches because doing it altogether was too much for my computer, the batch size could probably be larger than 3000, though
# doing it this way took about 2 hours I think
full_geocode = data.frame(parcel_num = NA, TLID = NA, distance = NA, numMatches = NA, matchType = NA, matchVars = NA)
full_geocode = full_geocode[-1,]
base = 1
while (base < nrow(PA17shp.nd)) {
  print(base)
  toRow = base+2999
  if (toRow>nrow(PA17shp.nd)) {
      toRow = nrow(PA17shp.nd)
  } 
  iter = PA17shp.nd[c(base:toRow),]
  geocode_iter = data.frame(geocode(toGeocode = iter,tgID = "parcel_num",refName = "Roads",geographies = c(),
                                    smallestGeo = "TLID",xy = T, matches = list(
                                      c("street_c","suffix_c","zip_c"),,
                                      c("street_c","suffix_c"),
                                      c("street_c","zip_c"),
                                      c("street_c")),
                                    refShpPath = roadsShpPath,
                                    refShpName = roadsShpName,
                                    refCSVPath = roadsCSVPath)[1])
  base = toRow+1
  full_geocode = rbind(full_geocode,geocode_iter)
}

# you may need to drop a couple of extraneous geocode fields at this point, but i think they probably get dropped later anyway
# things like distance and matchtype
PA17 = merge(PA17, full_geocode,by="parcel_num",all.x=T )




#------CREATING SSA WALKOVER-----####
# ssa stands for Strong Street Address match, although more accurately it is a weak match
# this creates a GIS_ID to GIS_ID file, connecting each GIS_ID to the lowest GIS_ID to which it matches based on overlapping street addresses
# it does multi-step matches, so if GISID1 is 1-5 Main St. and GISID2 is 3-9 Main St. and GISID3 is 9-11 Main St. 
# then all three will be matched to GISID1
# it connects to the lowest GIS_ID, because that way the ID will be more constant from year to year, since new IDs are higher

# expanding out the addresses
# this turns 1-5 Main St. into 3 rows: 1 Main St., 3 Main St., and 5 Main St. 
PA17.exp = expandAddresses(reference_raw =  PA17[,c("parcel_num","num1","num2","street_c","suffix_c","zip_c","GIS_ID")],ReferenceID = "parcel_num")

# then we make a unique street address field for each 
PA17.exp$sa = paste(PA17.exp$num1,PA17.exp$street_c,PA17.exp$suffix_c, PA17.exp$zip_c)

# this makes walkover_nc (no change) - this is the basic GIS_ID to GIS_ID dataset that contains all of the matches
# we then merge it onto itself over and over until nothing changes, in order to get multi-step matches like described above
# (multi step matches are quite rare)
# it does not allow matches between street addresses without numbers
# this is because there are a lot that seem to be vacant lots in different places on a street
# this is something that could be looked at more, but I think disallowing matches without numbers is smart
# and we might also want to disallow matches with number == 0
walkover_nc = PA17.exp[!is.na(PA17.exp$num1) ,c("sa","GIS_ID")]
walkover_nc = walkover_nc[!duplicated(walkover_nc),]
#merges with itself and then gets the lowest GIS_ID
walkover_nc = merge(walkover_nc,walkover_nc,by="sa")
walkover_nc = aggregate(GIS_ID.y ~ GIS_ID.x, walkover_nc,FUN = min)
names(walkover_nc)= c("GIS_ID1","GIS_ID2")
#at this point the walkover_nc is complete and will now be merged with itself iteratively

walkover1 = NULL
walkover2 = walkover_nc
i = 1
# iterate until there is no change and thus all steps have been taken, normally 1 or 2 rounds
while(!identical(walkover1,walkover2)) {
  print(paste("Round",i))
  walkover1 = walkover2
  # merge with itself and then take the lowest GIS_ID in the second column
  # while annotating this, there was no all.x=T and I just put that in which I think is correct
  # it had a , , which makes me think the all.x was deleted by mistake, but would be good to check 
  walkover2 = merge(rename(walkover1,intermediary = GIS_ID2), 
                    rename(walkover_nc,intermediary = GIS_ID1), all.x =T, 
                    by="intermediary")
  
  walkover2$intermediary = NULL
  walkover2 = aggregate(GIS_ID2 ~ GIS_ID1, walkover2,FUN = min)
  
  i = i + 1
}
# if the two were identical, that means that each GIS_ID cannot connect to a GIS_ID that is any lower, and the walkover is complete
walkover_ssa = walkover2

#checking it: making sure that every GIS_ID in PA17 is in the walkover (except those with no number)
sum(!PA17$GIS_ID %in% walkover_ssa$GIS_ID1 & 
      !is.na(PA17$num1) & 
      !is.na(PA17$GIS_ID))
#adding in GIS_IDs from those with no number, which will link back to themselves, making it complete
walkover_ssa = rbind(walkover_ssa,data.frame(GIS_ID1 = unique(PA17$GIS_ID[is.na(PA17$num1) & !PA17$GIS_ID %in% walkover_ssa$GIS_ID1]),
                                               GIS_ID2 = unique(PA17$GIS_ID[is.na(PA17$num1) & !PA17$GIS_ID %in% walkover_ssa$GIS_ID1])))
# doing some checks
# all GIS_IDs that are nonmissing should be in the walkover
sum(!PA17$GIS_ID %in% walkover_ssa$GIS_ID1 & !is.na(PA17$GIS_ID))
# there should be no duplicated GIS_IDs in the first column (although there should be some in the second)
sum(duplicated(walkover_ssa$GIS_ID1))

# the first number should be larger than the second 
length(unique(walkover_ssa$GIS_ID1))
length(unique(walkover_ssa$GIS_ID2))


#------ CORRECTING SSA WALKOVER WITH DISTANCES----####
# this now looks at how far apart each of the linked GIS_IDs are (linked based on street address above)
# this makes sure that we didn't link together two parcels that are really far away from each other
# it's possible it would be more efficient if we combined it with the part above, checking links as we made them, but this feels very clear

# we convert the projection to a planar projection so that we can use the gDistance function
P17shp.proj = spTransform(P17shp, CRS(planarProj))
P17shp.proj = P17shp.proj[ !duplicated(P17shp.proj@data$GIS_ID),]

walkover_ssa$distance = 0
# iterates through all rows where the GIS_ID is linked to a different one
for (row in row.names(walkover_ssa)[ walkover_ssa$GIS_ID1 != walkover_ssa$GIS_ID2]) {
  
  # making sure that both GIS_IDs are in the P17shp.proj file before doing gDistance, in order to avoid errors I was getting
  # this shouldn't happen, though, becuase all of the GIS_IDs shoudl be in the P17shp.proj, so this might be worthwhile to look into 
  # although I really don't think it's anything substantial...
  if (sum(!is.na(P17shp.proj@data$GIS_ID) & P17shp.proj@data$GIS_ID==walkover_ssa[row,"GIS_ID1"]) == 0 | 
        sum(!is.na(P17shp.proj@data$GIS_ID) & P17shp.proj@data$GIS_ID==walkover_ssa[row,"GIS_ID2"]) == 0)  {
    # if it can't find the IDs, it makes the distance = NA
    walkover_ssa[row,"distance"] = NA
  } else {
    
  # finds the distance and plugs it in
  walkover_ssa[row,"distance"] = gDistance(P17shp.proj[ !is.na(P17shp.proj@data$GIS_ID) & P17shp.proj@data$GIS_ID==walkover_ssa[row,"GIS_ID1"] ,],
                P17shp.proj[ !is.na(P17shp.proj@data$GIS_ID) & P17shp.proj@data$GIS_ID==walkover_ssa[row,"GIS_ID2"] ,])
  }
}
# ^^ABOVE^^ I didn't know about the byID option in gDistance, so it's possible there's a way to do this with fewer loops

# we have to choose a max distance, which if the distance is greater than, we will disallow the connection
# actually, in retrospect, it would probably be better to include the distance criteria when doing the first iterative connecting-based-on-street address part
  # becuase it's possible that a row connects to a close-by row (that it should be connected to) and the through that one connects to a far away one
  # and since the final distance will be too large, it will break the whole connection (including the first legitimate one), but this is definitely extremely rare
# now that we have the distance for each, we can look at how many connections we make depending on the max distance we choose
# thinking about this in retrospect, making the separate line file is probably not necessary, we just need a cumulative distribution with distance on the X axis
# distance is in meters
line = c()
for (minDistance in c(1:300)) {
  line[minDistance] = sum(walkover_ssa$GIS_ID1 != walkover_ssa$GIS_ID2 & walkover_ssa$distance<minDistance,na.rm = T)
}
# then we can plot it to see where there is an inflection point
# as was the case last year, it is at about 16 (not a coincidence since most have not changed, probably)
# for those with a distance greater than 16, their linked GIS_ID is changed to their original GIS_ID
plot(line)
walkover_ssa.d = walkover_ssa
walkover_ssa.d$GIS_ID2[!is.na( walkover_ssa.d$distance) & walkover_ssa.d$distance >16] = 
  walkover_ssa.d$GIS_ID1[!is.na( walkover_ssa.d$distance) & walkover_ssa.d$distance>16]
# at this point, walkover_ssa is a GIS_ID to Land_Parcel_ID walkover

walkover_ssa.d = rename(walkover_ssa.d, GIS_ID = GIS_ID1, Land_Parcel_ID = GIS_ID2)

# now we add it to the PA17 file 
PA17 = merge(PA17,walkover_ssa.d[!duplicated(walkover_ssa.d$GIS_ID),c("GIS_ID","Land_Parcel_ID")],by = "GIS_ID",all.x=T)


# write the properties file
propertiesVars = c("parcel_num","CM_ID","GIS_ID","ST_NUM","ST_NAME","ST_NAME_SUF","UNIT_NUM","ZIPCODE",
                 "LU","OWN_OCC","YR_BUILT","YR_REMOD","LAND_SF","GROSS_AREA","NUM_FLOORS",
                 "X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")
# i'm still not positive why a  few parcel nums are repeated in the property assessment file, i think it might be multiple structures owned together
#  it seems like it makes sense to drop them here, but keep them in the assessment file, favoring the one with larger GROSS_AREA
PA17.w = PA17.w[ order(PA17.w$parcel_num,-PA17.w$GROSS_AREA),]
write.csv(PA17.w[!duplicated(PA17.w$parcel_num),propertiesVars],propertiesPath,row.names=F)

# cleaning some variables before aggregating to land parcels
PA17$OWN_OCC_num = ifelse(PA17$OWN_OCC == "Y",1,0)
PA17$owner_address = paste(trim(PA17$MAIL_ADDRESS),trim(PA17$MAIL.CS),trim(PA17$MAIL_ZIPCODE),sep=" ")
PA17$LU_m = PA17$LU
PA17$LU_m[PA17$LU_m=="CC" | PA17$LU_m=="CM" | PA17$LU_m=="CP"] = "CD"

# aggregating to create the land parcels file
PA17.nd = PA17[!duplicated(PA17$parcel_num),]

# making the modal streets PAC file
# this finds the modal street and suffix combination for each Land_Parcel_ID
# this allows there to be 2 street names, in case an address is on a corner, and still do ranges for numbers
# otherwise, if we just took the range of numbers without separating based on street name, we might turn 1 Maple St. and 99 Beacon St. into 1-99 Maple St.
PA17$streetAndSuffix = paste(PA17$street_c,PA17$suffix_c)
modal_streets = aggregate(streetAndSuffix~Land_Parcel_ID, data=PA17,FUN=getTop)
n.obs <- sapply(modal_streets$streetAndSuffix, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(modal_streets$streetAndSuffix, "[", i = seq.max))
modal_streets.df = data.frame(mat,stringsAsFactors=F)
modal_streets.df$Land_Parcel_ID = modal_streets$Land_Parcel_ID
temp = data.frame(Land_Parcel_ID = modal_streets.df$Land_Parcel_ID)
for (num in seq.max) {
  var1name = paste("mstreetsuf_",num,sep="")
  var2name = paste("X",num,sep="")
  temp[,var1name] = modal_streets.df[,var2name]
}

PA17.modalstreet = merge(PA17,temp,by="Land_Parcel_ID",all.x=T)
rm(temp)

######Actually making the parcels file###
# aggregating the different variables that will be in the land parcels file
# a lot of these could be combined together, i just did it separately because I decided how to do each one at a time, and it felt clearer this way
# also some aggregate with the .nd (no duplicates file) and some aggregate with duplicates included
#number of aggregates
parcels = aggregate(parcel_num~Land_Parcel_ID, PA17.nd, FUN=length)
parcels = rename(parcels, num_properties = parcel_num)

#LU
parcels = merge(parcels,aggregate(LU~Land_Parcel_ID, PA17.nd, FUN = Mode),by="Land_Parcel_ID",all.x=T)

#OWN_OCC
parcels = merge(parcels,aggregate(OWN_OCC_num~Land_Parcel_ID, PA17.nd[PA17.nd$LU != "CM" & PA17.nd$LU != "CP", ], FUN = mean),by="Land_Parcel_ID",all.x=T)
parcels = rename(parcels, OWN_OCC = OWN_OCC_num)

#XY
PA17$XY = paste(PA17$X,PA17$Y,sep=", ")
parcels = merge(parcels, aggregate(XY~Land_Parcel_ID, PA17, FUN=Mode),by="Land_Parcel_ID",all.x=T)
parcels$X = gsub(",.+","",parcels$XY)
parcels$Y = gsub(".+, ","",parcels$XY)
parcels$XY<-NULL

# valuation and area data - i spent a little bit of time looking into this but it might be worthwhile to double check
# there are weird conventions around CDs, Exempts, sometimes when there are multiple properties in a single Land Parcel
# valuations are just all added together
parcels = merge(parcels, aggregate(cbind(AV_LAND,  AV_BLDG	)~Land_Parcel_ID, PA17.nd,FUN=sum,na.rm=T), by="Land_Parcel_ID",all.x=T)
# land is added together, excluding CDs
parcels = merge(parcels, aggregate(LAND_SF~Land_Parcel_ID, PA17.nd[ PA17.nd$LU != "CD",],FUN=sum,na.rm=T), by="Land_Parcel_ID",all.x=T)
# square footage is added together, including duplicates
parcels = merge(parcels, aggregate(GROSS_AREA~Land_Parcel_ID, PA17,FUN=sum,na.rm=T), by="Land_Parcel_ID",all.x=T)

parcels$ILratio = parcels$AV_LAND/parcels$AV_BLDG

#stories 
parcels = merge(parcels, aggregate(NUM_FLOORS~Land_Parcel_ID,PA17,FUN = Mode),by="Land_Parcel_ID",all.x=T)

#style
parcels = merge(parcels, aggregate(R_BLDG_STYL~Land_Parcel_ID,PA17,FUN = Mode),by="Land_Parcel_ID",all.x=T)

#owner address 
parcels = merge(parcels, aggregate(owner_address~Land_Parcel_ID,PA17,FUN=Mode),by="Land_Parcel_ID",all.x=T)

#yr built
parcels = merge(parcels,aggregate(YR_BUILT~Land_Parcel_ID,PA17,FUN=Mode),by="Land_Parcel_ID",all.x=T)

#yr_remod
parcels = merge(parcels,aggregate(YR_REMOD~Land_Parcel_ID,PA17,FUN=Mode),by="Land_Parcel_ID",all.x=T)

#estimate units
# condos are estimated using the number of properties, the others based on LU 
numCondos = aggregate(parcel_num~Land_Parcel_ID, PA17.nd[PA17.nd$LU == "CD" | PA17.nd$LU == "CC",], FUN = length)
numCondos = rename(numCondos, numCondos = parcel_num)
parcels = merge(parcels,numCondos,by="Land_Parcel_ID",all.x=T)
parcels$numUnits = ifelse(parcels$LU == "R1",1, 
                            ifelse(parcels$LU == "R2",2,
                                   ifelse(parcels$LU == "R3",3,
                                          ifelse(parcels$LU == "R4",4,
                                                 ifelse(parcels$LU == "A",7,
                                                        ifelse(parcels$LU == "CD" | parcels$LU == "CC" | parcels$LU == "CM" | parcels$LU == "CP", parcels$numCondos,NA))))))
                                                      

# this gets added to to create the full varnames list, it just adds num1_1, etc.
varnames = c("Land_Parcel_ID","num_properties","LU","numUnits","OWN_OCC","X","Y","AV_LAND","AV_BLDG","LAND_SF", "YR_BUILT","YR_REMOD",   
             "GROSS_AREA","ILratio","NUM_FLOORS","R_BLDG_STYL", "owner_address",
             "zip_c","Blk_ID_10", "BG_ID_10", "CT_ID_10","BRA_PD","NSA_NAME")

# this cycles through two modal streets, creating the street address components
for (num in c(1,2)) {
  varname = paste("mstreetsuf_",num,sep="")
  # gets the subset that is equal to the first (then second) modal street
  PAC_sub = PA17.modalstreet[ PA17.modalstreet[,varname] == PA17.modalstreet$streetAndSuffix,]
  PAC_sub_agg1 = sqldf("select Land_Parcel_ID,  min(num1) 'minNum1',max(num1) 'maxNum1',max(num2) 'maxNum2' from PAC_sub group by Land_Parcel_ID")
  PAC_sub_agg1$minNum = as.character(as.integer(PAC_sub_agg1$minNum1))
  PAC_sub_agg1$maxNum = as.character(pmax(as.integer(PAC_sub_agg1$maxNum1),as.integer(PAC_sub_agg1$maxNum2,na.rm = T)))
  PAC_sub_agg1= PAC_sub_agg1[,c("Land_Parcel_ID","minNum","maxNum")]
  names(PAC_sub_agg1)[2:3]<- paste(names(PAC_sub_agg1)[2:3],num,sep="_")
  # annoyingly not allowing multiple modes to be calculated at once, theres probably a way ive just been spending too long on this
  PAC_sub_agg2 = merge(merge( aggregate(street_c~Land_Parcel_ID,data = PAC_sub, FUN=Mode),
                       aggregate(suffix_c~Land_Parcel_ID,data = PAC_sub, FUN=Mode),by="Land_Parcel_ID",all.x=T),
                       aggregate(TLID~Land_Parcel_ID,data=PAC_sub, FUN=Mode), by="Land_Parcel_ID",all.x=T)
  
  names(PAC_sub_agg2)[2:4]<- paste(names(PAC_sub_agg2)[2:4],num,sep="_")
  parcels = merge(merge(parcels, PAC_sub_agg1, by="Land_Parcel_ID",all.x=T),PAC_sub_agg2, by="Land_Parcel_ID",all.x=T)
  varnames = c(varnames,paste(c("minNum_","maxNum_","street_c_","suffix_c_", "TLID_"),num,sep=""))
} 

#this should get reincorporated into the num 1, 2 loop but i'm just trying to do a quick fix right now
parcels$maxNum_1[ !is.na(parcels$maxNum_1) & !is.na(parcels$minNum_1) & parcels$maxNum_1 == parcels$minNum_1] = NA
parcels$maxNum_2[ !is.na(parcels$maxNum_2) & !is.na(parcels$minNum_2) & parcels$maxNum_2 == parcels$minNum_2] = NA

##get modal census IDs, zip, and neighborhoods
modal_geo = PA17 %>% group_by(Land_Parcel_ID) %>% summarise_each(funs(Mode(., na.rm = TRUE)),zip_c,TLID,Blk_ID_10, BG_ID_10, CT_ID_10,BRA_PD,NSA_NAME)
parcels = merge(parcels, modal_geo, by="Land_Parcel_ID")

# check all vars are there and drop any extras
parcels = parcels[,varnames]

#this should probably be incorporated back in earlier
parcels = rename(parcels, zip = zip_c, street_1 = street_c_1, suffix_1 = suffix_c_1, street_2 = street_c_2, suffix_2 = suffix_c_2)

#writing the parcels file
write.csv(parcels,LandParcelsCSVPath,row.names=F)


# creating the merged shapefile and writing it
P17shp = merge(P17shp,walkover_ssa.d[!duplicated(walkover_ssa.d$GIS_ID),c("GIS_ID","Land_Parcel_ID")],by="GIS_ID",all.x=T)
P17shp_new <- unionSpatialPolygons(P17shp, P17shp@data$Land_Parcel_ID )
newShpIDs = sapply(slot(P17shp_new, "polygons"), function(x) slot(x, "ID"))
P17shp_new = SpatialPolygonsDataFrame(P17shp_new,data.frame(row.names = newShpIDs,Land_Parcel_ID=newShpIDs))


#writing the parcels shapefile
writeOGR(P17shp_new,landParcelsShpPath ,landParcelsShpName,driver="ESRI Shapefile",overwrite_layer=TRUE)




##################################
#### Making the ID Connector ####
##################################
# having created the GIS_ID to Land_Parcel_ID connection, we need to connect all of the parcel_nums and property_IDs, including old ones, to the GIS_IDs 
# first we connect using IDs, then geographically

# getting all of the MAL records that are missing a parcel num or whose parcel num is not in the new PA file
# these will need to be connected geographically because we can't connect them using their parcel nums
# first we do an overlay of the P17shp file
malshp = mal[ !is.na(mal$Latitude) & (is.na(mal$parcel_num) | (!mal$parcel_num %in% PA17$parcel_num)),]
malshp$X = malshp$Longitude
malshp$Y = malshp$Latitude
coordinates(malshp) = ~Longitude+Latitude
proj4string(malshp) = latLongProj
malshp = spTransform(malshp, proj4string(P17shp))
malshp.over = over(malshp,P17shp)
malshp.over$parcel_num = malshp@data$parcel_num
malshp.over$Property_ID = malshp@data$Property_ID
malgeolink = malshp.over[,c("Property_ID","parcel_num","GIS_ID")]

# then we find the closest P17 polygon, for those mal cases that were not within one P17 parcel
malClosest = malshp[ malshp@data$Property_ID %in% malgeolink$Property_ID[ is.na(malgeolink$GIS_ID)],]
malClosest = spTransform(malClosest, planarProj)
P17shp = spTransform(P17shp, planarProj)

malgeolink2 = data.frame(Property_ID = malClosest@data$Property_ID, parcel_num = malClosest@data$parcel_num, GIS_ID=NA)
for (i in c(1:nrow(malClosest@data))) {
  # it first reduces to a subset of the P17, based on P17's centroid, more than enough to find the closest one, 
  # just so that the calculation doesn't take forever
  P17shpsub = P17shp[ ((P17shp@data$X - malClosest$X[i])^2 + (P17shp@data$Y - malClosest$Y[i])^2)<.00001, ]
  malgeolink2[i,"GIS_ID"] = P17shpsub@data$GIS_ID[which.min(gDistance(spgeom1 = malClosest[i,],spgeom2 = P17shpsub,byid =T,))]
}

# combines the two geographic links
malgeolink = rbind(malgeolink[ !is.na(malgeolink$GIS_ID),],malgeolink2)
sum(duplicated(malgeolink$Property_ID))
nrow(malgeolink)

P17shp = spTransform(P17shp, latLongProj)

# then we get all of the MAL records that can be connected using their parcel_num
malIDlink = merge(mal[ !is.na(mal$parcel_num),c("Property_ID","parcel_num") ], PA17[ !duplicated(PA17$parcel_num),c("parcel_num","GIS_ID")],by="parcel_num")

#combining them to have a full MAL walkover
malLink = rbind(malgeolink,malIDlink)

#checking that walkover for completeness
sum(!mal$Property_ID %in% malLink$Property_ID)
sum(!mal$parcel_num %in% malLink$parcel_num)




# next we connect the parcel_nums from the the old property assessment longitudinal file
# connecting old property assessment records geographically as well, first with an overlay, then finding the closest if that doesn't work
# this is a parcel_num to GIS_ID, no Property_ID
palshp = pal16[ !is.na(pal16$X) & 
                  (!pal16$parcel_num %in% PA17$parcel_num) &
                  (!pal16$parcel_num %in% malLink$parcel_num) &
                  !duplicated(pal16$parcel_num) ,]
palshp$Longitude = palshp$X
palshp$Latitude = palshp$Y
coordinates(palshp)=~Longitude+Latitude
proj4string(palshp) = latLongProj
palshp = spTransform(palshp, proj4string(P17shp))
palshp.over = over(palshp,P17shp)
palshp.over$parcel_num = palshp@data$parcel_num
palshp.over$Property_ID = NA
palgeolink = palshp.over[,c("Property_ID","parcel_num","GIS_ID")]

#get the subset that linked to nothing and find the closest P17 polygon, for them
palClosest = palshp[ palshp@data$parcel_num %in% palgeolink$parcel_num[ is.na(palgeolink$GIS_ID)],]
palClosest = spTransform(palClosest, planarProj)
P17shp = spTransform(P17shp, planarProj)

palgeolink2 = data.frame(Property_ID = NA, parcel_num = palClosest@data$parcel_num, GIS_ID=NA)
for (i in c(1:nrow(palClosest@data))) {
  P17shpsub = P17shp[ ((P17shp@data$X - palClosest$X[i])^2 + (P17shp@data$Y - palClosest$Y[i])^2)<.00001, ]
  palgeolink2[i,"GIS_ID"] = P17shpsub@data$GIS_ID[which.min(gDistance(spgeom1 = palClosest[i,],spgeom2 = P17shpsub,byid =T,))]
}
# create the full pal walkover
palgeolink = rbind(palgeolink[ !is.na(palgeolink$GIS_ID),],palgeolink2)
sum(duplicated(palgeolink$parcel_num))
nrow(palgeolink)
sum(!palgeolink$parcel_num %in% malLink$parcel_num)

# combine the mal and pal links
malPalLink = rbind(malLink,palgeolink[!palgeolink$parcel_num %in% malLink$parcel_num,])
sum(duplicated(malPalLink$Property_ID) & !is.na(malPalLink$Property_ID))
sum(!PA17$parcel_num %in% malPalLink$parcel_num)

# we are still missing the parcel nums that are within the pa17 csv file
PA17link = PA17[ !PA17$parcel_num %in% malPalLink$parcel_num,c("parcel_num","GIS_ID")]
PA17link$Property_ID = NA

# create the full link
fullLink = rbind(malPalLink,PA17link)

# and check it for completeness!
sum(!mal$parcel_num %in% fullLink$parcel_num)
sum(!mal$Property_ID %in% fullLink$Property_ID)
sum(!pal16$parcel_num %in% fullLink$parcel_num)
sum(!pal16$parcel_num %in% fullLink$parcel_num & !is.na(pal16$X))
sum(!PA17$parcel_num %in% fullLink$parcel_num)
sum(!PA17$GIS_ID %in% fullLink$GIS_ID)
sum(!walkover_ssa.d$GIS_ID %in% fullLink$GIS_ID)

# and for duplicates
sum(duplicated(fullLink$Property_ID))
sum(duplicated(fullLink$Property_ID) & !is.na(fullLink$Property_ID))

# get the GIS_ID to Land_Parcel_ID connection from walkover_ssa
fullLink.f = merge(fullLink, walkover_ssa.d[,c("GIS_ID","Land_Parcel_ID")],by="GIS_ID",all=T)


# write the walkover
write.csv(fullLink.f,IDConnectorPath,row.names=F)


## Ideas for the future
# Could improve the numbers in the roads file using the property assessment data


