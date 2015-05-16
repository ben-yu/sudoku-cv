(ns sudoku-cv.core)

(import '[org.opencv.core Mat Size CvType Core Scalar]
        '[org.opencv.highgui Highgui]
        '[org.opencv.imgproc Imgproc])

(def sudoku-src (Highgui/imread "resources/images/sudoku.jpg" 0))
(def blurred (Mat. (.size sudoku-src) CvType/CV_8UC3))
(def kernel
  (let [m (Mat. 3 3 CvType/CV_8UC1 (Scalar. 0 0))]
    (.setTo (.row m 1) (Scalar. 1 0))
    (.setTo (.col m 1) (Scalar. 1 0))
    m))

(defn preprocess [source dest]
  (Imgproc/GaussianBlur source dest (Size. 5 5) 0 0)
  (Imgproc/adaptiveThreshold dest dest 255 Imgproc/ADAPTIVE_THRESH_MEAN_C Imgproc/THRESH_BINARY 5 2)
  (Core/bitwise_not dest dest)
  (Imgproc/dilate dest dest kernel))

(Highgui/imwrite "resources/images/sudoku_blurred.png" blurred)
