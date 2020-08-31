{{ config(materialized='view') }}

 SELECT
	l_orderkey,
	sum(l_extendedprice*(1-l_discount)) AS revenue,
	o_orderdate,
	o_shippriority
FROM
	BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE,
	BUSINESSOBJECTS.ORDER_S_EXA_STAGE,
	BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
WHERE
	c_mktsegment = 'BUILDING'
	AND c_custkey = o_custkey
	AND l_orderkey = o_orderkey
	AND o_orderdate < DATE '1995-03-15'
	AND l_shipdate > DATE '1995-03-15'
GROUP BY
	l_orderkey,
	o_orderdate,
	o_shippriority
ORDER BY
	revenue DESC,
	o_orderdate
LIMIT 10