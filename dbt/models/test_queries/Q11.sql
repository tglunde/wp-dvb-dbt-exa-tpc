{{ config(materialized='view') }}

 SELECT
	ps_partkey,
	sum(ps_supplycost * ps_availqty) AS valuee
FROM
	--BUSINESSOBJECTS.PARTSUPP_S_EXA_STAGE,
	{{ source('accesslayer','partsupp')}},
	--BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE,
	{{ source('accesslayer','supplier')}},
	--BUSINESSOBJECTS.NATION_S_EXA_STAGE
	{{ source('accesslayer','nation')}}
WHERE
	ps_suppkey = s_suppkey
	AND s_nationkey = n_nationkey
	AND n_name = 'GERMANY'
GROUP BY
	ps_partkey
HAVING
	sum(ps_supplycost * ps_availqty) > (
	SELECT
		sum(ps_supplycost * ps_availqty) * 0.0001
	FROM
		BUSINESSOBJECTS.PARTSUPP_S_EXA_STAGE, BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE, BUSINESSOBJECTS.NATION_S_EXA_STAGE
	WHERE
		ps_suppkey = s_suppkey
		AND s_nationkey = n_nationkey
		AND n_name = 'GERMANY' )
ORDER BY
	valuee DESC