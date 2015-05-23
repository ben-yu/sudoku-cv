(ns sudoku-cv.neural-net
  (require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]))

(import '[org.opencv.core Mat MatOfInt Size CvType Core Scalar Point Rect]
        '[org.opencv.ml CvANN_MLP CvANN_MLP_TrainParams]
        '[org.opencv.highgui Highgui]
        '[org.opencv.imgproc Imgproc]
        '[java.util ArrayList])

(def input-size (* 28 28))
(def hidden-layers 16)
(def classes 10)

(def training-samples 42000)
(def test-samples 28000)

(def training-set (Mat. training-samples input-size CvType/CV_32F))
(def training-labels (Mat. training-samples classes CvType/CV_32F))

(def test-set (Mat. test-samples input-size CvType/CV_32F))
(def test-labels (Mat. test-samples classes CvType/CV_32F))


(def layers
  (let [m (Mat. 3 1 CvType/CV_32S)]
    (.put m 0 0 (int-array [input-size]))
    (.put m 1 0 (int-array [hidden-layers]))
    (.put m 2 0 (int-array [classes]))
    m))

(def neural-net (CvANN_MLP. layers CvANN_MLP/SIGMOID_SYM 0.6 1))

(def train-params (CvANN_MLP_TrainParams.))

(defn load-dataset [filename data labels]
  (with-open [in-file (clojure.java.io/reader filename)]
    (doseq [[row-num line] (map-indexed (fn [idx itm] [idx (apply map read-string (doall (csv/read-csv itm)))])
                                        (rest (line-seq in-file)))
          [col-num value] (map-indexed (fn [idx itm][idx itm]) line)]
        (if (= col-num 0)
          (.put labels row-num col-num (float-array [value]))
          (.put data row-num (- col-num 1) (float-array [value]))))))

(defn train [net input targets]
  (.train net input targets (Mat.)))

(load-dataset "resources/train.csv" training-set training-labels)
(train neural-net training-set training-labels)