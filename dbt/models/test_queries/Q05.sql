{{ config(materialized='view') }}

SELECT
	n_name,
	sum(l_extendedprice * (1 - l_discount)) AS revenue
FROM
	BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE,
	BUSINESSOBJECTS.ORDER_S_EXA_STAGE,
	BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE,
	BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE,
	BUSINESSOBJECTS.NATION_S_EXA_STAGE,
	BUSINESSOBJECTS.REGION_S_EXA_STAGE
WHERE
	c_custkey = o_custkey
	AND l_orderkey = o_orderkey
	AND l_suppkey = s_suppkey
	AND c_nationkey = s_nationkey
	AND s_nationkey = n_nationkey
	AND n_regionkey = r_regionkey
	AND r_name = 'ASIA'
	AND o_orderdate >= DATE '1994-01-01'
	AND o_orderdate < DATE '1994-01-01' + INTERVAL '1' YEAR
GROUP BY
	n_name
ORDER BY
	revenue DESC