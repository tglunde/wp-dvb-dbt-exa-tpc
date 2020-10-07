
truncate table DVB_CONFIG.API_HOOK_TEMPLATES ;
truncate table DVB_CONFIG.CONFIG ;
truncate table DVB_CONFIG.SYSTEM_COLORS ;
truncate table DVB_CONFIG.SYSTEM_DATA ;
truncate table DVB_CONFIG.AUTH_USERS ;
truncate table DVB_CONFIG.JOB_DATA ;
truncate table DVB_CONFIG.JOB_LOADS ;
truncate table DVB_CONFIG.JOB_SCHEDULES ;
truncate table DVB_CONFIG.JOB_SQL_QUERIES ;
truncate table DVB_CONFIG.JOB_TRIGGERS ;

import into DVB_CONFIG.API_HOOK_TEMPLATES from local csv file 'data/API_HOOK_TEMPLATES.sql';
import into DVB_CONFIG.CONFIG from local csv file 'data/CONFIG.sql';
import into DVB_CONFIG.SYSTEM_DATA from local csv file 'data/SYSTEM_DATA.sql';
import into DVB_CONFIG.SYSTEM_COLORS from local csv file 'data/SYSTEM_COLORS.sql';
import into DVB_CONFIG.AUTH_USERS from local csv file 'data/AUTH_USERS.sql';
import into DVB_CONFIG.JOB_DATA from local csv file 'data/JOB_DATA.sql';
import into DVB_CONFIG.JOB_LOADS from local csv file 'data/JOB_LOADS.sql';
import into DVB_CONFIG.JOB_SCHEDULES from local csv file 'data/JOB_SCHEDULES.sql';
import into DVB_CONFIG.JOB_SQL_QUERIES from local csv file 'data/JOB_SQL_QUERIES.sql';
import into DVB_CONFIG.JOB_TRIGGERS from local csv file 'data/JOB_TRIGGERS.sql';
