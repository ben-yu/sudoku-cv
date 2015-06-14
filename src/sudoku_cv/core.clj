(ns sudoku-cv.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [sudoku-cv.sudoku :as sudoku]))

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
  (Imgproc/threshold dest dest 30 255 Imgproc/THRESH_BINARY)
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
  (let [result (Mat. 25 25 CvType/CV_32FC1)]
    (do
      (preprocess-cell cell result)
      (.convertTo (find-cell-contour result) result CvType/CV_32FC1)
      (Imgproc/resize result result (Size. 25 25))
      result)))


(def templates (Mat. 9 1250 CvType/CV_32FC1))

(defn load-templates []
  (with-open [template-file (clojure.java.io/reader "resources/digit-templates.csv")]
    (doseq [[row-num line] (map-indexed (fn [idx itm] [idx (apply map read-string (doall (csv/read-csv itm)))])
                                        (line-seq template-file))
          [col-num value] (map-indexed (fn [idx itm][idx itm]) line)]
          (.put templates row-num col-num (float-array [(if (= value 1) 255 0)])))))


(def normalized-src (Highgui/imread "resources/images/sudoku_normalized.png" 0))

(defn match-template [candidate template]
  (let [result (Mat. 1 1 CvType/CV_32FC1)]
    (do
      (Imgproc/matchTemplate candidate template result Imgproc/TM_SQDIFF)
      (.minVal (Core/minMaxLoc result)))))

(defn digit-qualities
  [cell]
  (into {}
    (for [digit (range 9)]
      [(+ digit 1) (match-template cell (.reshape (.clone (.row templates digit)) 0 25))])))

(defn digit
  "Find the digit using template matching"
  [cell]
  (let [qualities (digit-qualities cell)]
    (if (every? #(> % 1.5E7) (vals qualities))
      0
      (key(apply min-key val qualities)))))

(defn solve [image]
  (sudoku/solve
    (partition 9
      (for [rect (cell-rects image)]
        (digit (resize (.submat normalized-src rect)))))))

(defn -main
  "Solves a sudoku puzzle from a picture"
  []
  (do
    (preprocess sudoku-src blurred)
    (solve
      (warp sudoku-src
            (curves-corners
              (approx-polygon
                (max-area-curve
                  (find-contours blurred))))
            (size-corners (.size sudoku-src))))))
