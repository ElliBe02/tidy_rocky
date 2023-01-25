# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT *
FROM \"mt_ev02\"
WHERE event_name LIKE 'STS_START' OR
  event_name LIKE 'STS_END' AND 
  timestamp >= '2022-05-21 00:00:00' AND
  timestamp <= '2022-06-21 23:59:59'
;
"""

mt_stsOperations = pd.read_sql(query, engine)

query = """
SELECT DISTINCT *
FROM \"mt_ev02\"
WHERE event_name LIKE 'AIS_ON' OR
  event_name LIKE 'AIS_OFF' AND 
  timestamp >= '2022-05-21 00:00:00' AND
   timestamp <= '2022-06-21 23:59:59'
;
"""

mt_dark_activity = pd.read_sql(query, engine)


query = """
SELECT DISTINCT *
FROM \"ihs_stsOperations\"
WHERE movementdate >= '2022-05-21 00:00:00' AND
  movementdate <= '2022-06-21 23:59:59'
;
"""

ihs_stsOperations = pd.read_sql(query, engine)

query = """
SELECT *
FROM \"ihs_darkActivity\"
WHERE movementdate >= '2022-05-21 00:00:00' AND
  movementdate <= '2022-06-21 23:59:59'
;
"""

ihs_darkActivity = pd.read_sql(query, engine)


