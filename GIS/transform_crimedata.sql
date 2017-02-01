CREATE TABLE tmp_points AS
SELECT
  id,
  ST_SetSRID(ST_MakePoint(longitude, latitude), 4326) as location
FROM crime_staging;  

CREATE TABLE tmp_location AS
SELECT t.id, n.gid
FROM tmp_points t, nyct2010 n
WHERE ST_Within(t.location, n.geom);

INSERT INTO crimes
(cmplnt_num, cmplnt_fr_dt, cmplnt_fr_tm, cmplnt_to_dt, cmplnt_to_tm, rpt_dt, ky_cd, ofns_desc, pd_cd, pd_desc, crm_atpt_cptd_cd, law_cat_cd, juris_desc, boro_nm, addr_pct_cd, loc_of_occur_desc, prem_typ_desc, parks_nm, hadevelopt, x_coord_cd, y_coord_cd, latitude, longitude, lat_lon, location, loc_gid)
SELECT
	cmplnt_num,
	cmplnt_fr_dt,
	cmplnt_fr_tm,
	cmplnt_to_dt,
	cmplnt_to_tm,
	rpt_dt,
	ky_cd,
	ofns_desc,
	pd_cd,
	pd_desc,
	crm_atpt_cptd_cd,
	law_cat_cd,
	juris_desc,
	boro_nm,
	addr_pct_cd,
	loc_of_occur_desc,
	prem_typ_desc,
	parks_nm,
	hadevelopt,
	x_coord_cd,
	y_coord_cd,
	CASE WHEN latitude != 0 THEN latitude END,
	CASE WHEN longitude != 0 THEN longitude END,
	lat_lon,
	CASE
		WHEN longitude != 0 AND latitude != 0
		THEN ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)
	END,
	tmp_location.gid
FROM
  crime_staging
    LEFT JOIN tmp_location ON crime_staging.id = tmp_location.id;

TRUNCATE TABLE crime_staging;
DROP TABLE tmp_points;
DROP TABLE tmp_location;
