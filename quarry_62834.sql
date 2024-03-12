/*
see https://quarry.wmcloud.org/query/62834
with thanks to Peter Coombe
returns % of articles in WikiProject with given language equivalent
*/

WITH t AS 
( 
SELECT 
  ll_lang, article.page_id AS pid
FROM page talk
  INNER JOIN page article ON talk.page_title = article.page_title
  INNER JOIN categorylinks ON cl_from = talk.page_id
  LEFT JOIN langlinks ON ll_from = article.page_id
WHERE
  talk.page_namespace = 1 AND
  article.page_namespace = 0 AND
  cl_to IN ( 
    "Top-importance_Computational_Biology_articles",
    "High-importance_Computational_Biology_articles",
    "Mid-importance_Computational_Biology_articles",
    "Low-importance_Computational_Biology_articles",
    "NA-importance_Computational_Biology_articles",
    "Unknown-importance_Computational_Biology_articles"
  )
)
SELECT 
  ll_lang AS lang, 
  COUNT( pid ) AS articles,
  ROUND( COUNT( pid ) * 100 / ( SELECT COUNT( DISTINCT pid ) FROM t ), 2 ) AS percent_of_en
FROM t
GROUP BY ll_lang
ORDER BY articles DESC;
