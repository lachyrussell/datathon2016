USE datathon2016
DROP TABLE job_impressions
CREATE TABLE job_impressions
(
	job_id	int
,	user_id	int
,	session_id	int
,	search_id	int
,	search_ranking	tinyint
,	mobile_user	FLOAT
,	created_at	DATETIME
)
GO

BULK INSERT job_impressions
FROM 'C:\Users\lachyrussell\Desktop\Datathon\All\job_impressions_all.csv'
WITH
(
	MAXERRORS = 0,
	FIRSTROW = 2,
	FIELDTERMINATOR = '\t',
	ROWTERMINATOR = '\n'
)
GO

SELECT *
FROM job_impressions
GO
