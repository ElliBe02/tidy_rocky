# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT DISTINCT 
  imolrorihsnumber AS imo, 
  longitude, 
  latitude, 
  countrycode, 
  countryname, 
  movementdate, 
  facilitytype, 
  facilityid
FROM \"ihs_portCalls\"
WHERE movementdate >= '2022-05-21 00:00:00'
  AND movementdate <= '2022-06-21 23:59:59'
;
"""

ihs_port_calls = pd.read_sql(query, engine)
