{{ config(materialized='view') }}

SELECT
	o_orderpriority,
	count(*) AS order_count
FROM
	BUSINESSOBJECTS.ORDER_S_EXA_STAGE
WHERE
	o_orderdate >= DATE '1993-07-01'
	AND o_orderdate < DATE '1993-07-01' + INTERVAL '3' MONTH
	AND EXISTS (
	SELECT
		*
	FROM
		BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
	WHERE
		l_orderkey = o_orderkey
		AND l_commitdate < l_receiptdate )
GROUP BY
	o_orderpriority
ORDER BY
	o_orderpriority