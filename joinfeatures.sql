USE datathon2016
DROP TABLE joinfeatures
SELECT 
A.job_id
,A.title
,A.location_id
,A.salary_type
,A.salary_min
,A.salary_max
,A.raw_job_type
,A.abstract
,B.avg_mobile_user
,C.queries
,A.hat
INTO
joinfeatures
FROM 
jobs_all A LEFT JOIN joinmobile2 B

ON A.job_id = B.job_id

LEFT JOIN joinfeatures3 C
ON A.[job_id] = C.[job_id]

order by job_id

;WITH CTE AS(
   SELECT job_id, salary_type, salary_min, salary_max, raw_job_type, abstract, avg_mobile_user, queries, hat,
       RN = ROW_NUMBER()OVER(PARTITION BY job_id ORDER BY job_id)
   FROM dbo.joinfeatures
   )
DELETE FROM CTE WHERE RN > 1
