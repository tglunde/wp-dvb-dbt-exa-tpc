{{ config(materialized='view') }}

WITH tpch_res AS (
 SELECT
	l_suppkey AS supplier_no,
	sum(l_extendedprice * (1 - l_discount)) AS total_revenue
FROM
	--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
	{{ source('accesslayer','lineitem')}}
WHERE
	l_shipdate >= DATE '1996-01-01'
	AND l_shipdate < DATE '1996-01-01' + INTERVAL '3' MONTH
GROUP BY
	l_suppkey)

SELECT
	s_suppkey,
	s_name,
	s_address,
	s_phone,
	total_revenue
FROM
	--BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE,
	{{ source('accesslayer','supplier')}},
	tpch_res
WHERE
	s_suppkey = supplier_no
	AND total_revenue = (
	SELECT
		max(total_revenue)
	FROM
		tpch_res )
ORDER BY
	s_suppkey;