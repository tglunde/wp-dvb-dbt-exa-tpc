{{ config(materialized='view') }}

SELECT
	c_name,
	c_custkey,
	o_orderkey,
	o_orderdate,
	o_totalprice,
	sum(l_quantity) AS sum_quantity
FROM
	--BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE,
	{{ source('accesslayer','customer')}},
	--BUSINESSOBJECTS.ORDER_S_EXA_STAGE,
	{{ source('accesslayer','order')}},
	--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
	{{ source('accesslayer','lineitem')}}
WHERE
	o_orderkey IN (
	SELECT
		l_orderkey
	FROM
		--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
		{{ source('accesslayer','lineitem')}}
	GROUP BY
		l_orderkey
	HAVING
		sum(l_quantity) > 300 )
	AND c_custkey = o_custkey
	AND o_orderkey = l_orderkey
GROUP BY
	c_name,
	c_custkey,
	o_orderkey,
	o_orderdate,
	o_totalprice
ORDER BY
	o_totalprice DESC,
	o_orderdate
LIMIT 100