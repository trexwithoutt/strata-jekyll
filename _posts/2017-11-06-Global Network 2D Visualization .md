---
title: "Global 2D Network Visualization"
author: "REX(RUIZHE) ZHOU"
categories: project
---

**Introduction**

I found an intresting visualization project recently, the whole idea is created by [Matt Leonawicz](https://github.com/leonawicz/mapmate)
This project is basic using of `save_map` function to create network and display the path way

**Requirement**

- `R 3.6`
- package `mapmate`
- package `animation`

**Output**

[<img src="/images/network2d1.gif" class="fit image">]({{ "/images/network2d.gif" | "https://github.com/trexwithoutt/trexwithoutt.github.io/blob/master" }})


**Programming**

```r
library(mapmate)
library(dplyr)
 
set.seed(192)
data(network)
 
distFun <- function(x) 1 - x/max(x)  # simple inverse distance weighting
endpoints <- gc_endpoints(network, "lon", "lat")
```

```r
# take a weighted sample, e.g., favoring larger averaged populations and
# shorter distances
endpoints <- mutate(endpoints, Dist_wts = distFun(Dist))
endpoints <- sample_n(endpoints, 500, replace = TRUE, weight = (Pop_wts0 + Pop_wts1)/2 + 
    Dist_wts)
 
# expand data frame from endpoints to arcs, each composed of a sequence of
# points
arcs_flat <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1", breakAtDateLine = TRUE)
arcs_globe <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")
```

```r
paths_flat <- gc_paths(arcs_flat, "group", size = 5)
paths_globe <- gc_paths(arcs_globe, "group", size = 5)
```

```r
n <- max(paths_flat$id)
png.args <- list(width = 600, height = 300, bg = "black")
clrs <- c("#1E90FF50", "#FFFFFF50", "#FFFFFF", "#1E90FF75")
ylm <- range(paths_flat$lat)  # trimming empty southern map region
```

```r
gglist <- save_seq(paths_flat, id = "id", n.frames = n, ortho = FALSE, type = "network", 
    ylim = ylm
    , png.args = png.args, save.plot = FALSE, return.plot = TRUE)
 
library(animation)
# you may need to specify a different path on your Windows machine you may
# also need to ensure convert.exe is part of your particular installation
saveGIF(for (i in seq_along(gglist)) print(gglist[[i]]), "network2D.gif", interval = 1/20, 
    ani.width = 600, ani.height = 300)
```
