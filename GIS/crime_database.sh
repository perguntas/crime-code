#!/bin/bash

# requires the .sql files create_crime_schema, add_newark_airport and transform_crimedata
# requires the download of the NYPD Complaint Map Data at https://data.cityofnewyork.us

createdb -U root crime-data

psql -U root -d crime-data -f create_crime_schema.sql

shp2pgsql -s 2263:4326 nyct2010_15b/nyct2010.shp | psql -U root -d crime-data
psql -U root -d crime-data -f add_newark_airport.sql
psql -U root -d nyc-taxi-data -c "VACUUM ANALYZE nyct2010;"

schema="(cmplnt_num, cmplnt_fr_dt, cmplnt_fr_tm, cmplnt_to_dt, cmplnt_to_tm, rpt_dt, ky_cd, ofns_desc, pd_cd, pd_desc, crm_atpt_cptd_cd, law_cat_cd, juris_desc, boro_nm, addr_pct_cd, loc_of_occur_desc, prem_typ_desc, parks_nm, hadevelopt, x_coord_cd, y_coord_cd, latitude, longitude, lat_lon)"

psql -U root -d crime-data -c "COPY crime_staging ${schema} FROM 'D:\crime-data\NYPD_Complaint_Map__Year_to_Date_.csv' CSV HEADER;"
psql -U root -d crime-data -f transform_crimedata.sql

psql -U root -d crime-data -c "COPY crime_staging ${schema} FROM 'D:\crime-data\NYPD_Complaint_Map__Historic_.csv' CSV HEADER;"
psql -U root -d crime-data -f transform_crimedata.sql
