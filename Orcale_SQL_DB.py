#cx_Oracle
import cx_Oracle
   dsn_tns = cx_Oracle.makedsn('host', 'port', service_name='give service name') 
   conn = cx_Oracle.connect(user='username', password='password', dsn=dsn_tns) 
   c = conn.cursor()
   c.execute('select count(*) from schema.table_name limit 0,30')
for row in c:
   print row
conn.close()
