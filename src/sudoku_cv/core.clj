(ns sudoku-cv.core
  (require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]))

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

(defn is-rect? [curve]
  (= (.size curve) (Size. 1 4)))

(defn size-corners
  "Finds corners of a rectangle"
  [size]
  (MatOfPoint2f.
    (into-array
      [(Point. 0 0)
      (Point. (.width size) 0)
      (Point. 0 (.height size))
      (Point. (.width size) (.height size))])))

(defn preprocess
  "Blur -> Threshold -> Invert the source image"
  [source dest]
  (Imgproc/GaussianBlur source dest (Size. 11 11) 0 0)
  (Imgproc/adaptiveThreshold dest dest 255 Imgproc/ADAPTIVE_THRESH_MEAN_C Imgproc/THRESH_BINARY 5 2)
  (Core/bitwise_not dest dest)
  (Imgproc/dilate dest dest kernel))


; (defn fill-areas [source color]
;   "Fills regions, and returns region with largest area"
;   (let [mask (Mat. (+ (.height (.size source)) 2) (+ (.width (.size source)) 2) CvType/CV_8UC1 (Scalar. 0 0))]
;     (key
;       (apply max-key val
;              (into {}
;                    (for [x (range 0 (.width (.size source)))
;                          y (range 0 (.height (.size source)))]
;                     (if (and
;                           (.get source x y)
;                           (> (aget (.get source x y) 0) 0.0))
;                             [(Point. y x) (Imgproc/floodFill source mask (Point. x y) color)])))))))

(defn find-contours
  "List of contours in an image"
  [source]
  (let [contours (ArrayList.)]
    (Imgproc/findContours source contours (MatOfPoint2f.) Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_SIMPLE)
    contours))

(defn max-area-curve
  "Curve with largest area"
  [curves]
  (key
    (apply max-key val
           (into {}
                 (for [curve curves]
                   [curve (Imgproc/contourArea curve)])))))

(defn approx-polygon
  "Douglas–Peucker algorithm: Finds a curve with fewer points"
  [curve]
  (let [curve2f (MatOfPoint2f.)
        approxCurve (MatOfPoint2f.)]
    (.fromList curve2f (.toList curve))
    (Imgproc/approxPolyDP curve2f approxCurve (* (Imgproc/arcLength curve2f true) 0.02) true)
    approxCurve))


(defn curves-corners
  "Finds bounding box of a curve"
  [curve]
  (let [result (MatOfPoint2f.)
        points (.toList curve)
        sum-sorted (sort-by #(+ (.x %1) (.y %1)) points)
        diff-sorted (sort-by #(- (.y %1) (.x %1)) points)]
    (.fromList result [(first sum-sorted) (first diff-sorted) (last diff-sorted) (last sum-sorted)])
    result))


(defn warp
  "Warps an image from one perspective to another"
  [source src-corners dest-corners]
  (let [dest (Mat.)
        transformMat (Imgproc/getPerspectiveTransform src-corners dest-corners)]
    (Imgproc/warpPerspective source dest transformMat (.size source))
    dest))



(defn preprocess-cell
  "Preprocess individual sudoku cells"
  [source dest]
  (Imgproc/equalizeHist source dest)
  (Imgproc/threshold dest dest 80 255 Imgproc/THRESH_BINARY)
  (Core/bitwise_not dest dest))

(defn find-cell-contour [cell]
  (let [original (Mat.)]
    (do
      (.copyTo cell original)
      (.submat original (Imgproc/boundingRect (max-area-curve (find-contours cell)))))))

(defn cell-size [img-size]
  (Size. (/ (.width img-size) 9)
         (/ (.height img-size) 9)))

(defn cell-rects [source]
  (let [rect-size (cell-size (.size source))
        cell-width (int (.width rect-size))
        cell-height (int (.height rect-size))]
    (for [i (range 81)]
      (Rect. (* (int (mod i 9)) cell-width)
             (* (int (/ i 9)) cell-height)
             cell-width
             cell-height))))


(defn resize [cell]
  (let [net-input (Mat. 25 25 CvType/CV_32FC1)]
    (do
      (preprocess-cell cell cell)
      (.convertTo (find-cell-contour cell) net-input CvType/CV_32FC1)
      (Imgproc/resize net-input net-input (Size. 25 25))
      net-input)))


(defn match-template [candidate template]
  (let [result (Mat. 1 1 CvType/CV_32FC1)])
  (do
    (Imgproc/matchTemplate candidate template result Imgproc/TM_SQDIFF)
    (.minVal (Core/minMaxLoc result))))


(def templates (Mat. 9 1250 CvType/CV_32FC1))

(defn load-templates []
  (with-open [template-file (clojure.java.io/reader "resources/digit-templates.csv")]
    (doseq [[row-num line] (map-indexed (fn [idx itm] [idx (apply map read-string (doall (csv/read-csv itm)))])
                                        (line-seq template-file))
          [col-num value] (map-indexed (fn [idx itm][idx itm]) line)]
          (.put templates row-num col-num (float-array [(if (= value 1) 255 0)])))))


(def normalized-src (Highgui/imread "resources/images/sudoku_normalized.png" 0))

(defn -main
  "This should be pretty simple."
  []
  (do
    (preprocess sudoku-src blurred)
    (Highgui/imwrite "resources/images/sudoku_normalized.png"
      (warp sudoku-src (curves-corners (approx-polygon (max-area-curve (find-contours blurred))))
        (size-corners (.size sudoku-src))))))
