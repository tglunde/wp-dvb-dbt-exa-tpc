{{ config(materialized='view') }}

SELECT
	p_brand,
	p_type,
	p_size,
	count(DISTINCT ps_suppkey) AS supplier_cnt
FROM
	--BUSINESSOBJECTS.PARTSUPP_S_EXA_STAGE,
	{{ source('accesslayer','partsupp')}},
	--BUSINESSOBJECTS.PART_S_EXA_STAGE
	{{ source('accesslayer','part')}}
WHERE
	p_partkey = ps_partkey
	AND p_brand <> 'Brand#45'
	AND p_type NOT LIKE 'MEDIUM POLISHED%'
	AND p_size IN (49,
	14,
	23,
	45,
	19,
	3,
	36,
	9)
	AND ps_suppkey NOT IN (
	SELECT
		s_suppkey
	FROM
		--BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE
		{{ source('accesslayer','supplier')}}
	WHERE
		s_comment LIKE '%Customer%Complaints%' )
GROUP BY
	p_brand,
	p_type,
	p_size
ORDER BY
	supplier_cnt DESC,
	p_brand,
	p_type,
	p_size