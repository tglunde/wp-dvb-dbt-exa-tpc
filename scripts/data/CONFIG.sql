version_branch,release,"branch of datavaultbuillder",0
server_name,Datavaultbuilder,"Server name",0
datetime_format,"DD.MM.YYYY HH24:MI:SS","date and time format for output in gui",0
date_format,DD.MM.YYYY,"date format for output in gui",0
staging_load_async,TRUE,"sync or async load",1
datavault_preload_parent,TRUE,"preload hubs with sat and link loads, links with linksats",1
default_loading_batch_size,50000,"batch size for staging load",1
default_staging_load_log_progress_update_size,10000,"in staging, defines after how many rows a progress update will be logged",1
token_timeout,300,"login user management in seconds",0
inactivity_timeout,7200,"login user management in seconds",0
leave_site_popup,FALSE,"show popup when leave the site or reload",0
polling_time_staging,5000,"polling frequency for load state refresh in  milliseconds",0
polling_time_datavault,5000,"polling frequency for load state refresh in milliseconds",0
datavault_key_type,hash_bytea,"key type for the datavault 'hash_bytea', 'hash_uuid', 'bk'",1
polling_time_lineage,1801,"polling frequency for load state refresh in milliseconds",0
data_preview_timeout,300,"query timeout for data preview in seconds",0
support_link,http://datavault-builder.com/en/supportchat,"for link in gui",0
include_in_accesslayer_default,FALSE,"automatically include new business ruleset in accesslayer",1
use_bulk_copy_on_mssql,TRUE,"if client db is mssql, do loads as bulk loads. No function on postgres",1
max_parallel_loads,5,"global maximum of parallelly running staging + datavault loads",1
max_parallel_staging,3,"global maximum of parallelly running staging loads",1
max_parallel_loads_from_staging_table,-1,"global maximum of parallelly running load on the same staging table, set to -1 for no limit",1
run_job_sql_query_on_core,TRUE,"if true, run job triggered sql query on core, otherwise run on clientdb",1
transaction_link_fk_constraints,TRUE,"transaction links have foreign key constraints on multi link hash keys",1
environment_color,"#eeeeee","sets the background color of the navigation bar - can be used to distinguish environments",0
environment_text_color,"#000000","sets the text color of the navigation bar - can be used to distinguish environments",0
environment_text_color_active,"##3F51B5","sets the selected text color of the navigation bar - can be used to distinguish environments",0
version_major,5,"major version of datavaultbuilder",0
version_minor,3,"minor version of datavaultbuilder",0
version_revision,0,"revision of datavaultbuillder",0
version_build,1,"build of datavaultbuillder",0
enable_beta_features,FALSE,"displays beta features in the gui",0
datavault_load_sat_in_batches,TRUE,"Load Current Satellite tables in batches",1
datavault_load_sat_batch_size,10000000,"Max batch size (approximation only, number of batches will be the next higher power of 2)",1
datavault_load_commit_batches,TRUE,"Commit each batch during satellite load",1
transaction_link_parent_hub_constraints,TRUE,"transaction links have foreign key constraints on parent hub",1
