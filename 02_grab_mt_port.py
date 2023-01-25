# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT DISTINCT 
  imo, 
  timestamp_utc, 
  port_id,
  country_code
FROM \"mt_historical_port_calls\"
WHERE timestamp_utc >= '2022-05-21 00:00:00'
  AND timestamp_utc <= '2022-06-21 23:59:59'
;
"""

mt_port_calls = pd.read_sql(query, engine)
