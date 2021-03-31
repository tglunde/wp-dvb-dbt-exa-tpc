{{ config(materialized='view') }}

SELECT
	sum(l_extendedprice) / 7.0 AS avg_yearly
FROM
	--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE,
	{{ source('accesslayer','lineitem')}},
	--BUSINESSOBJECTS.PART_S_EXA_STAGE
	{{ source('accesslayer','part')}}
WHERE
	p_partkey = l_partkey
	AND p_brand = 'Brand#23'
	AND p_container = 'MED BOX'
	AND l_quantity < (
	SELECT
		0.2 * avg(l_quantity)
	FROM
		--BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
		{{ source('accesslayer','lineitem')}}
	WHERE
		l_partkey = p_partkey )