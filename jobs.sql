USE datathon2016
DROP TABLE jobs_all
CREATE TABLE jobs_all
(
	job_id	int
,	title	varchar(238)
,	raw_location	varchar(255)
,	location_id	varchar(42)
,	subclasses	smallint
,	salary_type	varchar(1)
,	salary_min	int
,	salary_max	int
,	raw_job_type	varchar(217)
,	abstract	NTEXT
,	Segment	varchar(7)
,	hat	smallint
)

GO
BULK INSERT jobs_all
FROM 'C:\Users\lachyrussell\Desktop\Datathon\All\jobs_all.csv'
WITH
(	
	MAXERRORS = 0,
	FIRSTROW = 2,
	FIELDTERMINATOR = '\t',
	ROWTERMINATOR = '\n'
)
GO

SELECT *
FROM jobs_all
GO