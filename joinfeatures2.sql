USE datathon2016
DROP TABLE joinfeatures2
SELECT 
A.job_id
,C.query
INTO
joinfeatures2
FROM 
job_searches C LEFT JOIN job_impressions B

ON C.user_id = B.user_id
AND C.search_id = B.search_id
INNER JOIN jobs_all A
ON B.job_id = A.job_id
order by job_id

DROP TABLE joinfeatures3
Select distinct ST2.job_id, 
    substring(
        (
            Select ', '+ST1.query  AS [text()]
            From joinfeatures2 ST1
            Where ST1.job_id = ST2.job_id
            ORDER BY ST1.job_id
            For XML PATH ('')
        ), 2, 1000) [queries]
		
INTO joinfeatures3
From jobs_all ST2