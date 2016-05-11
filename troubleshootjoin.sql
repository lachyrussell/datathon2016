USE datathon2016
DROP TABLE troubleshootjoin

SELECT 
A.job_id
,A.title
,A.abstract
,A.location_id
,A.hat
INTO 
troubleshootjoin
FROM 
jobs_all A INNER JOIN troubleshoot B

ON A.job_id = B.job_id
GO
