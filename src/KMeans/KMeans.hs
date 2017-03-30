{-# LANGUAGE ParallelListComp #-}

module KMeans.KMeans (
  kmeans
) where

import Data.List      (sortBy, unwords)
import Data.Function  (on)
import Debug.Trace    (trace)

type Point     = [Double]
type Cluster   = [Point]

-- Debug flag that controls 'traceD' debug statements.
debug = False
traceD = if debug then trace else (\x y -> y)

sortCps :: [(Int, Point)] -> [(Int, Point)]
sortCps
  = sortBy (compare `on` fst)

kmeans :: Int -> [Point] -> [Cluster]
kmeans 0 _
  = error "K-Means clustering requires at least one cluster. Given none."
kmeans k []
  = replicate k []
kmeans k points@(p : ps)
  = kmeans' initialCentroids initialCentroidPointMapping
  where
    dim = length p
    initialCentroids = genInitialCentroids k dim
    initialCentroidPointMapping = recluster initialCentroids points

-- Arbitrarily generate k real valued vectors of dimension dim.
genInitialCentroids :: Int -> Int -> [Point]
genInitialCentroids 0 _
  = []
genStartCentroids k dim
  = centroid : genStartCentroids (k - 1) dim
  where
    centroid = [fromIntegral (1 + k) .. fromIntegral (dim + k)]

-- Takes list of centroids and list of points and produces the clusters.
kmeans' :: [Point] -> [(Int, Point)] -> [Cluster]
kmeans' centroids clustersToPoints
  | newCentroids == centroids  = mkClusters clustersToPoints
  | otherwise                  = kmeans' newCentroids newClustersToPoints
  where
    -- newcps = update mapping according to centroids§
    newClustersToPoints = recluster (map snd clustersToPoints) centroids

    -- update centroids according to the updated mappings
    newCentroids = findClusterMeans newClustersToPoints

-- Given a list of centroids and a list of points, clusters the points according
-- to the centroids i.e. returns a list of: (index of closest centroid, point).
recluster :: [Point] -> [Point] -> [(Int, Point)]
recluster ps cs
  = [ (closestCentroid cs p, p) | p <- ps ]

mkClusters :: [(Int, Point)] -> [Cluster]
mkClusters cps
  = undefined

-- Given a list of centriods and a point, returns the index in the list of the
-- closest centroid
closestCentroid :: [Point] -> Point -> Int
closestCentroid (c : cs) p
  = closestCentroid' 0 1 (euclidDist c p) cs
  where
    closestCentroid' :: Int -> Int -> Double -> [Point] -> Int
    closestCentroid' minIdx _ _ []
      = minIdx
    closestCentroid' minIdx nextIdx leastDist (c : cs)
      | dist < leastDist = traceD debug $ closestCentroid' nextIdx (nextIdx + 1) dist cs
      | otherwise        = traceD debug $ closestCentroid' minIdx (nextIdx + 1) leastDist cs
      where
        dist  = euclidDist c p
        debug = unwords ["closestCentroid\'", show minIdx, show nextIdx, show leastDist, show (c:cs)]

-- Produces a list of the new centroids from the means of each cluster. clusters
-- are given in the form of the (cluster index, point) pairs.
-- FIXME: If a cluster had no points mapping to it, it is lost as no mean for it
--        is computed. The problem should probably be fixed before hitting this
--        function.
findClusterMeans :: [(Int, Point)] -> [Point]
findClusterMeans cps@((c, p) : _)
   = findClusterMeans (sortCps cps) 0 1 p
   where
     -- Divides each component of the point by a single integer value.
     dividePointByInt :: Point -> Int -> Point
     dividePointByInt p n
       = let n' = fromIntegral n in [ x / n' | x <- p ]

     {-
       First argument is list of (cluster index, point) pairs that have been
       ordered so that they appear in asecnding order of cluster index.

       Second argument is the index of the last cluster whose mean was being
       computed. This is always less than or equal to the cluster at the
       beginning of the list.

       Third argument is the number of points so far in this cluster.

       Fourth argument is the sum of the points in the cluster so far.

       Given an empty list, the returned value is the singleton list containing
       the mean of the current cluster (computed from the other arguments).

       Otherwise, there are two cases. The head of the list either refers to the
       current cluster, or we have traversed through all the points of the
       cluster and now the head refers to the next cluster.

       In the case where the current cluster's mean is still being computed,
       keep the index of the current cluster the same; add one to the number of
       points in this cluster; and add the current point to the sum so far.

       In the case where the head of the lists represents the start of the next
       cluster, compute the mean of the current cluster and recurse. The index
       of the current cluster is now the index found in the head of the list;
       the number of points in the cluster so far is one; and the sum of the
       cluster so far is simply the point at the head of the list.

       There is surely a much simpler design than this...
     -}
     findClusterMeans :: [(Int, Point)] -> Int -> Int -> Point -> [Point]
     findClusterMeans [] _ numThisCluster sumThisCluster
       = [sumThisCluster `dividePointByInt` numThisCluster]
     findClusterMeans ((c, p) : cps) currClusterIdx numThisCluster sumThisCluster
       | c == currClusterIdx = findClusterMeans cps currClusterIdx (numThisCluster + 1) newSum
       | otherwise           = meanOfCluster : findClusterMeans cps c 1 p
       where
         newSum        = [ x + s | x <- p | s <- sumThisCluster]
         meanOfCluster = sumThisCluster `dividePointByInt` numThisCluster

-- Returns the Euclidean distance between two points.
euclidDist :: Point -> Point -> Double
euclidDist p q
  = (sqrt . sum) $ zipWith (\i i' -> (i'- i) ^ 2) p q
