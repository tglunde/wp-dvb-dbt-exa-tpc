
#Create schema and tables
/datadrive/EXAplus-6.2.3/exaplus -c localhost:8888 -u sys -p exasol -f ~/sql_dump.sql 

#Import data 
/datadrive/EXAplus-6.2.3/exaplus -c localhost:8888 -u sys -p exasol -s dbt -f /datadrive/import.sql


