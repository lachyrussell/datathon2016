USE datathon2016
DROP TABLE joinmobile
SELECT 
A.job_id
,B.mobile_user
INTO
joinmobile
FROM 
jobs_all A INNER JOIN job_impressions B

ON A.job_id = B.job_id

order by job_id

DROP TABLE joinmobile2
SELECT job_id,
AVG(mobile_user) AS avg_mobile_user
 INTO
joinmobile2
  FROM joinmobile 
 GROUP BY job_id
 ORDER BY
 job_id