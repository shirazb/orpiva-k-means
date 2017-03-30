module KMeans.KMeans (
  kmeans
) where

import Data.List      (sortBy, minimumBy, unwords)
import Data.Function  (on)
import Debug.Trace    (trace)

type Attribute = Double
type Point     = [Attribute]
type Cluster   = [Point]

debug = False

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
mkClusters = undefined

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
        dist    = euclidDist c p
        debug = unwords ["closestCentroid\'", show minIdx, show nextIdx, show leastDist, show (c:cs)]

findClusterMeans :: [(Int, Point)] -> [Point]
findClusterMeans = undefined

-- Returns the Euclidean distance between two points.
euclidDist :: Point -> Point -> Double
euclidDist p q
  = (sqrt . sum) $ zipWith (\i i' -> (i'- i) ^ 2) p q

traceD = if debug then trace else (\x y -> y)
