# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT DISTINCT imo
FROM \"mt_historical_port_calls\"
WHERE timestamp_utc >= '2022-05-21 00:00:00'
AND timestamp_utc <= '2022-06-21 23:59:59'
ORDER BY imo
;
"""

mt_01 = pd.read_sql(query, engine)


query = """
SELECT DISTINCT imolrorihsnumber AS imo
FROM \"ihs_portCalls\"
WHERE movementdate >= '2022-05-21' 
    AND movementdate < '2022-06-22'
ORDER BY imo
;
"""

ihs_01 = pd.read_sql(query, engine)
