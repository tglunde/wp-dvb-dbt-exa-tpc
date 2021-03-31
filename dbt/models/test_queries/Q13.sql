{{ config(materialized='view') }}

SELECT
	c_count,
	count(*) AS custdist
FROM
	(
	SELECT
		c_custkey, count(o_orderkey)
	FROM
		--BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE
		{{ source('accesslayer','customer')}}
	--LEFT OUTER JOIN BUSINESSOBJECTS.ORDER_S_EXA_STAGE ON
	LEFT OUTER JOIN {{ source('accesslayer','order')}} ON
		c_custkey = o_custkey
		AND o_comment NOT LIKE '%special%requests%'
	GROUP BY
		c_custkey )AS c_orders (c_custkey,
	c_count)
GROUP BY
	c_count
ORDER BY
	custdist DESC,
	c_count DESC