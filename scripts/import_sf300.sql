import into dbt.lineitem from local csv file '/datadrive/sf300/lineitem.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.orders from local csv file '/datadrive/sf300/orders.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.customer from local csv file '/datadrive/sf300/customer.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.part from local csv file '/datadrive/sf300/part.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.partsupp from local csv file '/datadrive/sf300/partsupp.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.supplier from local csv file '/datadrive/sf300/supplier.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.nation from local csv file '/datadrive/sf300/nation.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;

import into dbt.region from local csv file '/datadrive/sf300/region.tbl' encoding='UTF-8' row separator='LF' column separator='|' column delimiter='"' skip=0 reject limit 0;
