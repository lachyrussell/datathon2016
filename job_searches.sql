USE datathon2016
DROP TABLE job_searches
CREATE TABLE job_searches
(
	user_id	int
,	search_id	int
,	raw_location	varchar(320)
,	location_id	varchar(13)
,	latitude	float
,	longitude	float
,	query	varchar(331)
,	mobile_user	tinyint
,	created_at	DATETIME
)
GO

BULK INSERT job_searches
FROM 'C:\Users\lachyrussell\Desktop\Datathon\All\job_searches_all.csv'
WITH
(
	MAXERRORS = 0,
	FIRSTROW = 2,
	FIELDTERMINATOR = '\t',
	ROWTERMINATOR = '\n'
)
GO

SELECT *
FROM job_searches
GO