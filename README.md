# amazon-recs
I participated in a hackathon organized by Amazon Analytics Lab in collaboration with my MSBA Program at UCLA. Our task is to recommend a similar list of items when given an item. We were given a datast containing 20K observations of different types of products on Amazon, which can be found in the data/Unprocessed folder. 

We grouped all products by its product_category and created a text-based recommendation system using NLP (TF-IDF transformation and cosine similarity matrices) for each category. We created the model based on the furniture category, which can be found in furniture.R, but we also tested the model on clothing and grocery.

Apart from the unprocessed dataset, we also simulated some additional attributes such as the price range, rating, and sponsorship status of products. This can be found in data/Processed/furniture_text_cat.csv. I created some visualizations of the recommendation outputs as well as some additional categorical filters based on these simulated variables to further narrow down the recommendations. 
