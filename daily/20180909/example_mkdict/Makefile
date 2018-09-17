default: 00 01

# https://medium.com/eureka-engineering/%E5%9F%BA%E7%A4%8E%E7%B7%A8-elasticsearch%E3%81%AE%E6%A4%9C%E7%B4%A2%E3%82%AF%E3%82%A8%E3%83%AA%E3%82%92%E4%BD%BF%E3%81%84%E3%81%93%E3%81%AA%E3%81%9D%E3%81%86-ace3e18c2174

# # (例8-1 数値でのスコアリング
# curl -XGET "http://localhost:9200/sample_20161206/recipes/_search" -d'
# {
#   "query": {
#     "function_score": {
#       "query": {
#         "bool": {
#           "must": [
#             {
#               "term": {
#                 "prep_time_min": 15
#               }
#             }
#           ]
#         }
#       }
#     }
#   }
# }'
00:
	dictknife mkdict query/function_score/query/bool/must/0/term/prev_time_min 15 | tee 00.json


# #（例9 tagにvegetarianが含まれているレシピの中で、
# # 調理時間が10分以内なら10点、
# # 準備時間が20分以内なら10点、
# # 材料の中ににんにくがあれば5点
# # スコアの高い順版に並べ替える
# curl -XGET "http://localhost:9200/sample_20161206/recipes/_search" -d'
# {
#   "query": {
#     "function_score": {
#       "query": {
#         "bool": {
#           "must": [
#             {
#               "term": {
#                 "tags": "vegetarian"
#               }
#             }
#           ]
#         }
#       },
#       "score_mode": "sum",
#       "boost_mode": "multiply",
#       "functions": [
#         {
#           "filter": {
#             "term": {
#               "cook_time_min": 10
#             }
#           },
#           "weight": 10
#         },
#         {
#           "filter": {
#             "range": {
#               "prep_time_min": {
#                 "lte": 20
#               }
#             }
#           },
#           "weight": 10
#         },
#         {
#           "filter": {
#             "match": {
#               "ingredients": "garlic"
#             }
#           },
#           "weight": 5
#         }
#       ]
#     }
#   }
# }'

01:
	dictknife mkdict query/function_score/query/bool/must/0/term/tags vegetarian query/function_score/score_mode sum query/function_score/boost_mode multiply query/function_score/functions/0/filter/term/cook_time_min 10 query/function_score/functions/0/filter/weight 10 query/function_score/functions/1/filter/range/prep_time_min/lte 20 query/function_score/functions/1/weight 10 query/function_score/functions/2/filter/match/ingredients garlic query/function_score/functions/2/weight 5 | tee 02.json
