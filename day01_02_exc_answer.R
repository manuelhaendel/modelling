library(ncdf4)

prcp <- nc_open(file.path(getwd(), "day01/material/prcp_daily_2000-2000.nc"))
tmax <- nc_open(file.path(getwd(), "day01/material/tmax_daily_2000-2000.nc"))
tmin <- nc_open(file.path(getwd(), "day01/material/tmin_daily_2000-2000.nc"))

str(tmin)


prcp_ras <- raster(prcp)
