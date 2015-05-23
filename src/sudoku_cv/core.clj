(ns sudoku-cv.core)

(import '[org.opencv.core Mat MatOfPoint2f Size CvType Core Scalar Point Rect]
        '[org.opencv.highgui Highgui]
        '[org.opencv.imgproc Imgproc]
        '[java.util ArrayList])

(def sudoku-src (Highgui/imread "resources/images/sudoku.jpg" 0))
(def sudoku-mask (Mat. (+ (.height (.size sudoku-src)) 2) (+ (.width (.size sudoku-src)) 2) CvType/CV_8UC1 (Scalar. 0 0)))
(def blurred (Mat. (.size sudoku-src) CvType/CV_8UC3))
(def kernel
  (let [m (Mat. 3 3 CvType/CV_8UC1 (Scalar. 0 0))]
    (.setTo (.row m 1) (Scalar. 1 0))
    (.setTo (.col m 1) (Scalar. 1 0))
    m))

(defn preprocess [source dest]
  (Imgproc/GaussianBlur source dest (Size. 11 11) 0 0)
  (Imgproc/adaptiveThreshold dest dest 255 Imgproc/ADAPTIVE_THRESH_MEAN_C Imgproc/THRESH_BINARY 5 2)
  (Core/bitwise_not dest dest)
  (Imgproc/dilate dest dest kernel))

(defn fill-areas [source color]
  (let [mask (Mat. (+ (.height (.size source)) 2) (+ (.width (.size source)) 2) CvType/CV_8UC1 (Scalar. 0 0))]
    (key
      (apply max-key val
             (into {}
                   (for [x (range 0 (.width (.size source)))
                         y (range 0 (.height (.size source)))]
                    (if (and
                          (.get source x y)
                          (> (aget (.get source x y) 0) 0.0))
                            [(Point. y x) (Imgproc/floodFill source mask (Point. x y) color)])))))))

(defn find-contours [source]
  (let [contours (ArrayList.)]
    (Imgproc/findContours source contours (MatOfPoint2f.) Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_SIMPLE)
    contours))

(defn max-area-curve [curves]
  (key
    (apply max-key val
           (into {}
                 (for [curve curves]
                   [curve (Imgproc/contourArea curve)])))))

(defn approx-polygon [curve]
  (let [curve2f (MatOfPoint2f.)
        approxCurve (MatOfPoint2f.)]
    (.fromList curve2f (.toList curve))
    (Imgproc/approxPolyDP curve2f approxCurve (* (Imgproc/arcLength curve2f true) 0.02) true)
    approxCurve))

(defn is-rect? [curve]
  (= (.size curve) (Size. 1 4))

(defn curves-corners [curve]
  (let [result (MatOfPoint2f.)
        points (.toList curve)
        sum-sorted (sort-by #(+ (.x %1) (.y %1)) points)
        diff-sorted (sort-by #(- (.y %1) (.x %1)) points)]
    (.fromList result [(first sum-sorted) (first diff-sorted) (last diff-sorted) (last sum-sorted)])
    result))

(defn size-corners [size]
  (MatOfPoint2f.
    (into-array
      [(Point. 0 0)
      (Point. (.width size) 0)
      (Point. 0 (.height size))
      (Point. (.width size) (.height size))])))

(defn cell-size [img-size]
  (Size. (/ (.width img-size) 9)
         (/ (.height img-size) 9)))

(defn cell-rects [source]
  (let [rect-size (cell-size (.size source))
        cell-width (int (.width rect-size))
        cell-height (int (.height rect-size))]
    (for [i (range 81)]
      (Rect. (* (mod i 3) cell-width)
             (* (/ i 3) cell-height)
             cell-width
             cell-height))))

(defn warp [source src-corners dest-corners]
  (let [dest (Mat.)
        transformMat (Imgproc/getPerspectiveTransform src-corners dest-corners)]
    (Imgproc/warpPerspective source dest transformMat (.size source))
    dest))



(preprocess sudoku-src blurred)
(Highgui/imwrite "resources/images/sudoku_normalized.png"
                 (warp sudoku-src (curves-corners (approx-polygon (max-area-curve (find-contours blurred))))
                       (size-corners (.size sudoku-src))))