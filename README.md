# Hierarchical-cluster-cancer

The following project was an attempt to cluster data on cervical cancer. How many groups we can clusters, and what is the optimum number of clusters.

Hierarchical clustering collects similar vectors after clusters of similar vectors are formed, the process repeats itself by making a bigger cluster compromising of the smaller cluster. The end cluster is the aggregate cluster consisting all the nested clusters.
This process is useful in assessing how similar samples are to each other. We can also determine optimum number of clusters by using looking at average silehotte width
![selection_criterion](https://user-images.githubusercontent.com/31741251/129242750-0c12f934-bb60-49e7-9cb2-db110518b8b7.png)

As we can see that optimal number of cluster is 2. So we base our clustering in taking 2 clusters
![opti_clusters](https://user-images.githubusercontent.com/31741251/129243527-059c897a-daf5-4daa-a684-96c816e3d05c.png)

We get  the following this might represent cancer patients from non cancer patients

