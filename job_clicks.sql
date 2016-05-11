USE datathon2016
DROP TABLE job_clicks
CREATE TABLE job_clicks
(job_id INT, user_id INT, session_ID INT, search_id INT, created_at DATETIME)
GO

BULK INSERT job_clicks
FROM 'C:\Users\lachyrussell\Desktop\Datathon\All\job_clicks_all.csv'
WITH
(
	MAXERRORS = 0,
	FIRSTROW = 2,
	FIELDTERMINATOR = '\t',
	ROWTERMINATOR = '\n'
)
GO

SELECT *
FROM job_clicks
GO

