import into dbt.lineitem from local csv file '/datadrive/sf1/lineitem.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.orders from local csv file '/datadrive/sf1/orders.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.customer from local csv file '/datadrive/sf1/customer.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.part from local csv file '/datadrive/sf1/part.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.partsupp from local csv file '/datadrive/sf1/partsupp.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.supplier from local csv file '/datadrive/sf1/supplier.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.nation from local csv file '/datadrive/sf1/nation.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.region from local csv file '/datadrive/sf1/region.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;
