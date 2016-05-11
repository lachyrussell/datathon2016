USE datathon2016
DROP TABLE joindisperfect2
SELECT 
A.job_id
,C.query
INTO
joindisperfect2
FROM 
job_searches C LEFT JOIN job_impressions B

ON C.user_id = B.user_id
AND C.search_id = B.search_id
INNER JOIN jobs_all A
ON B.job_id = A.job_id
order by job_id

DROP TABLE joindisperfect3
Select distinct ST2.job_id, 
    substring(
        (
            Select ', '+ST1.query  AS [text()]
            From joinfeatures2 ST1
            Where ST1.job_id = ST2.job_id
            ORDER BY ST1.job_id
            For XML PATH ('')
        ), 2, 1000) [queries]
		
INTO joindisperfect3
From jobs_all ST2

DROP TABLE joindisperfect
SELECT 
A.job_id
,E.queries
,A.title
,A.abstract
,G.search_ranking
,A.location_id
,A.hat
INTO
joindisperfect

FROM 
job_clicks F INNER JOIN job_impressions G

ON F.job_id = G.job_id
AND F.user_id = G.user_id
AND F.session_id = G.session_id
AND F.search_id = G.search_id

LEFT JOIN joindisperfect3 E
ON F.[job_id] = E.[job_id]

LEFT JOIN jobs_all A
ON A.[job_id] = E.[job_id]

WHERE search_ranking <41 AND HAT =0

order by job_id

;WITH CTE AS(
   SELECT job_id, queries, title, abstract, search_ranking, hat,
       RN = ROW_NUMBER()OVER(PARTITION BY job_id ORDER BY job_id)
  FROM dbo.joindisperfect
   )
DELETE FROM CTE WHERE RN > 1
