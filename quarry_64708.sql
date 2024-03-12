/*
see https://quarry.wmcloud.org/query/64708
with thanks to Peter Coombe
returns computational biology articles with Spanish language equivalent
*/

WITH t AS 
( 
SELECT 
  ll_lang, article.page_title AS en_title, ll_title AS es_title
FROM page talk
  INNER JOIN page article ON talk.page_title = article.page_title
  INNER JOIN categorylinks ON cl_from = talk.page_id
  LEFT JOIN langlinks ON ll_from = article.page_id
WHERE
  ll_lang = "es" AND
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
  ll_lang AS lang, en_title, es_title
FROM t
