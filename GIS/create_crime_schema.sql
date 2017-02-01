CREATE EXTENSION postgis;

CREATE TABLE crime_staging (
	id serial primary key,
	cmplnt_num integer,
	cmplnt_fr_dt varchar,
	cmplnt_fr_tm varchar,
	cmplnt_to_dt varchar,
	cmplnt_to_tm varchar,
	rpt_dt varchar,
	ky_cd integer,
	ofns_desc varchar,
	pd_cd integer,
	pd_desc varchar,
	crm_atpt_cptd_cd varchar,
	law_cat_cd varchar,
	juris_desc varchar,
	boro_nm varchar,
	addr_pct_cd integer,
	loc_of_occur_desc varchar,
	prem_typ_desc varchar,
	parks_nm varchar,
	hadevelopt varchar,
	x_coord_cd integer,
	y_coord_cd integer,
	latitude numeric,
	longitude numeric,
	lat_lon varchar
);

CREATE TABLE crimes (
	id serial primary key,
	cmplnt_num integer,
	cmplnt_fr_dt varchar,
	cmplnt_fr_tm varchar,
	cmplnt_to_dt varchar,
	cmplnt_to_tm varchar,
	rpt_dt varchar,
	ky_cd integer,
	ofns_desc varchar,
	pd_cd integer,
	pd_desc varchar,
	crm_atpt_cptd_cd varchar,
	law_cat_cd varchar,
	juris_desc varchar,
	boro_nm varchar,
	addr_pct_cd integer,
	loc_of_occur_desc varchar,
	prem_typ_desc varchar,
	parks_nm varchar,
	hadevelopt varchar,
	x_coord_cd integer,
	y_coord_cd integer,
	latitude numeric,
	longitude numeric,
	lat_lon varchar,
	loc_gid integer
);

SELECT AddGeometryColumn('crimes', 'location', 4326, 'POINT', 2);

