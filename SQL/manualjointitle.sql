USE datathon2016
DROP TABLE manualjointitle
CREATE TABLE manualjointitle
(
title	varchar(238)
)

GO
BULK INSERT manualjointitle
FROM 'C:\Users\lachyrussell\Desktop\Final\Variables\ngram.txt'
WITH
(	
	MAXERRORS = 0,
	FIRSTROW = 1,
	FIELDTERMINATOR = ',',
	ROWTERMINATOR = '\n'
)
GO
