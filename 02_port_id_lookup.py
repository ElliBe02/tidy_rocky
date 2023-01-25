# python connection object

import pandas as pd
import sqlalchemy

# import chunk
engine = sqlalchemy.create_engine('postgresql://postgres:admin@127.0.0.1:5432/rocky')

query = """
SELECT DISTINCT *
FROM \"ihs_facilities_ports\"
WHERE port_id IS NOT NULL AND 
  unlocode IS NOT NULL
;
"""

port_id_lookup = pd.read_sql(query, engine)
