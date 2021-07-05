# Image Compressor in Haskell

The Purpose of this project is to make an image compressor using the k-means clustering algorithm

https://en.wikipedia.org/wiki/K-means_clustering

## Usage:

```
make
```

```
./imageCompressor n e file.txt

n being the number of colors
e being the convergence limit
```

## Example:

```
./imageCompressor 2 0.8 examples/ex.txt
```
Original file:

```
(0, 0) (66, 20, 26)
(0, 1) (98, 99, 233)
(0, 2) (45, 12, 167)
(1, 0) (33, 16, 94)
(1, 1) (78, 8, 9)
(1, 2) (1, 56, 37)
(2, 0) (88, 77, 211)
(2, 1) (1, 56, 37)
(2,2) (15,89,40)
```

Compressed file:

```
--
(32,41,34)
-
(0,0) (66,20,26)
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (1,56,37)
(2,1) (1,56,37)
(2,2) (15,89,0)
--
(77,63,204)
-
(0,1) (98,99,233)
(0,2) (45,12,167)
(2,0) (88,77,211)
```

## With image parser using my algorithm
 
Original image:

<p align="center">
<img src=https://https://github.com/CzesiaLa/RoadToHaskell/tree/main/04-image_compressor/examples/ds.jpg alt="original"><br/>

K = 2

<p align="center">
<img src=https://https://github.com/CzesiaLa/RoadToHaskell/tree/main/04-image_compressor/examples/dscomp.png alt="compressed"><br/>

