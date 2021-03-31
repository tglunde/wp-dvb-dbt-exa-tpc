{{ config(materialized='view') }}

SELECT
	sum(l_extendedprice*l_discount) AS revenue
FROM
	--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
	{{ source('accesslayer','lineitem')}}
WHERE
	l_shipdate >= DATE '1994-01-01'
	AND l_shipdate < DATE '1994-01-01' + INTERVAL '1' YEAR
	AND l_discount BETWEEN 0.06 - 0.01 AND 0.06 + 0.01
	AND l_quantity < 24