{{ config(materialized='view') }}

SELECT
	c_custkey,
	c_name,
	sum(l_extendedprice * (1 - l_discount)) AS revenue,
	c_acctbal,
	n_name,
	c_address,
	c_phone,
	c_comment
FROM
	BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE,
	BUSINESSOBJECTS.ORDER_S_EXA_STAGE,
	BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE,
	BUSINESSOBJECTS.NATION_S_EXA_STAGE
WHERE
	c_custkey = o_custkey
	AND l_orderkey = o_orderkey
	AND o_orderdate >= DATE '1993-10-01'
	AND o_orderdate < DATE '1993-10-01' + INTERVAL '3' MONTH
	AND l_returnflag = 'R'
	AND c_nationkey = n_nationkey
GROUP BY
	c_custkey,
	c_name,
	c_acctbal,
	c_phone,
	n_name,
	c_address,
	c_comment
ORDER BY
	revenue DESC
LIMIT 20