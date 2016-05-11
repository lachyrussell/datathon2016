USE datathon2016
DROP TABLE troubleshoot
CREATE TABLE troubleshoot
(
	job_id	int
,	HAT	float
)

GO
BULK INSERT troubleshoot
FROM 'C:\Users\lachyrussell\Desktop\Datathon\R_predictionQuery.csv'
WITH
(	
	MAXERRORS = 0,
	FIRSTROW = 2,
	FIELDTERMINATOR = ',',
	ROWTERMINATOR = '\n'
)
GO

