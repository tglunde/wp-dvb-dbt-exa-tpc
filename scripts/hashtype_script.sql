CREATE LUA SCRIPT "CHANGE_TYPE" () RETURNS ROWCOUNT AS
res = query([[SELECT CONSTRAINT_SCHEMA, CONSTRAINT_TABLE, CONSTRAINT_NAME, COLUMN_NAME, REFERENCED_SCHEMA, REFERENCED_TABLE, REFERENCED_COLUMN 
			  FROM sys.EXA_ALL_CONSTRAINT_COLUMNS 
			  WHERE CONSTRAINT_SCHEMA = 'DATAVAULT' AND CONSTRAINT_TYPE = 'FOREIGN KEY']])

query([[OPEN SCHEMA DATAVAULT]])
for i=1, #res do
tbl_name=join(".", quote(res[i].CONSTRAINT_SCHEMA), quote(res[i].CONSTRAINT_TABLE))
cons_name=quote(res[i].CONSTRAINT_NAME)
output('table: '..tbl_name)
query([[alter table ::t drop constraint ::c]], {t=tbl_name, c=cons_name})
end

cols = query([[SELECT distinct COLUMN_SCHEMA, COLUMN_TABLE, COLUMN_NAME 
			   FROM sys.EXA_ALL_COLUMNS 
			   WHERE column_type like 'CHAR(32) UTF8' and column_schema like 'DATAVAULT' and column_object_type = 'TABLE']])
for i=1, #cols do
tbl=join(".", quote(cols[i].COLUMN_SCHEMA), quote(cols[i].COLUMN_TABLE))
col=quote(cols[i].COLUMN_NAME)
query([[ALTER TABLE ::t MODIFY (::c HASHTYPE)]], {t=tbl, c=col})
end

for i=1, #res do
tbl_name=join(".", quote(res[i].CONSTRAINT_SCHEMA), quote(res[i].CONSTRAINT_TABLE))
cons_name=quote(res[i].CONSTRAINT_NAME)
cons_col=res[i].COLUMN_NAME
ref_tbl=join(".", quote(res[i].REFERENCED_SCHEMA), quote(res[i].REFERENCED_TABLE))
ref_col=res[i].REFERENCED_COLUMN
query([[ALTER TABLE ::t ADD CONSTRAINT ::cn FOREIGN KEY(::cc) REFERENCES ::rt(::rc)]], {t=tbl_name, cn=cons_name, cc=cons_col, rt=ref_tbl, rc=ref_col})
end