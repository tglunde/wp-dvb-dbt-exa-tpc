{{ config(materialized='view') }}

SELECT
	s_name,
	count(*) AS numwait
FROM
	--BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE,
	{{ source('accesslayer','supplier')}},
	--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE l1,
	{{ source('accesslayer','lineitem')}} l1,
	--BUSINESSOBJECTS.ORDER_S_EXA_STAGE,
	{{ source('accesslayer','order')}},
	--BUSINESSOBJECTS.NATION_S_EXA_STAGE
	{{ source('accesslayer','nation')}}
WHERE
	s_suppkey = l1.l_suppkey
	AND o_orderkey = l1.l_orderkey
	AND o_orderstatus = 'F'
	AND l1.l_receiptdate > l1.l_commitdate
	AND EXISTS (
	SELECT
		*
	FROM
		--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE l2
		{{ source('accesslayer','lineitem')}} l2
	WHERE
		l2.l_orderkey = l1.l_orderkey
		AND l2.l_suppkey <> l1.l_suppkey )
	AND NOT EXISTS (
	SELECT
		*
	FROM
		--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE l3
		{{ source('accesslayer','lineitem')}} l3
	WHERE
		l3.l_orderkey = l1.l_orderkey
		AND l3.l_suppkey <> l1.l_suppkey
		AND l3.l_receiptdate > l3.l_commitdate )
	AND s_nationkey = n_nationkey
	AND n_name = 'SAUDI ARABIA'
GROUP BY
	s_name
ORDER BY
	numwait DESC,
	s_name