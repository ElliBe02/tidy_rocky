# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT facilityid
FROM \"ihs_portCalls\"
WHERE movementdate >= '2022-05-21 00:00:00' AND
   movementdate <= '2022-06-21 23:59:59'
;
"""

mapping_ihs_ports_facilityid = pd.read_sql(query, engine)

query = """
SELECT countryname, facilityid 
FROM \"ihs_portCalls\"
WHERE movementdate >= '2022-05-21 00:00:00' AND
      movementdate <= '2022-06-21 23:59:59'
;
"""

mapping_ihs_ports_facilityidii = pd.read_sql(query, engine)


query = """
SELECT countryname AS region, COUNT(DISTINCT imolrorihsnumber) AS "ihs.imo_count"
FROM \"ihs_portCalls\"
WHERE  movementdate >= '2022-05-21 00:00:00' 
  AND movementdate <= '2022-06-21 23:59:59'

GROUP BY countryname

;
"""

ihs_imo_count_country = pd.read_sql(query, engine)
