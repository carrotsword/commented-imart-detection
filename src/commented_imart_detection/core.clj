(ns commented-imart-detection.core
  (:use [clojure.pprint]
        [spyscope.core]))

(defrecord NumberedLine [number data])

(defn type-n [data] {:data data :type :normal})

(defn type-c [data] {:data data :type :comment})

(defn is-n? [t] (= :normal (:type t)))

(defn is-c? [t] (= :comment (:type t)))

(defn analyse
  [parsed c]
  (let [curr (last parsed)
        line (str (:data curr) c)]
    (concat (butlast parsed)
            (cond (is-n? curr) (if (.endsWith line "<!--")
                                 [(type-n (subs line 0 (- (count line) 4))) (type-c "<!--")]
                                 [(type-n line)])
                  (is-c? curr) (if (.endsWith line "-->")
                                 [(type-c line) (type-n "")]
                                 [(type-c line)])))))

(defn analyze-line
  [prev-status line]
  (let [initial-data (if (= prev-status :comment) (type-c "") (type-n ""))]
    (reduce analyse [initial-data] line )))

(defn analyzed-item
  [parsed-data nl]
  {:data parsed-data
   :line-number (:number nl)
   :end-type (if (is-n? (last parsed-data)) :normal :comment)})

(defn analyze-and-collect [analyzed-arr nl]
  (conj analyzed-arr
        (analyzed-item  (analyze-line (:end-type (last analyzed-arr)) (:data nl))
                        nl)))

(defn analyze-html
  [numbered-lines]
  (reduce analyze-and-collect [] numbered-lines))

(defn add-line-number
  [lseq]
  (map (fn [a b] (NumberedLine. a b)) (iterate inc 1) lseq))

(defrecord Result [file numbered-line])

(defn contains-imart?
  [numbered-line]
  (let [string (.toLowerCase (:data numbered-line))]
    (or (<= 0 (.indexOf string "<imart"))
        (<= 0 (.indexOf string "</imart"))
        (<= 0 (.indexOf string "imart>")))))

(defn flatten-analyzed-items
  [analyzed-items]
  (reduce (fn [coll parsed]
            (->> (:data parsed)
                 (map (fn [data] (assoc data :line-number (:line-number parsed))))
                 (into coll)))
          []
          analyzed-items))

(defn extract-imart-comment
  [lseq]
  (->> (add-line-number lseq)
       (analyze-html)
       (flatten-analyzed-items)
       (filter is-c?)
       (filter contains-imart?)))

(defn extract-imart-comment-from-file
  [file]
  (with-open [r (clojure.java.io/reader file)]
    (map (fn [item] (Result. (.getAbsolutePath file) item))
         (extract-imart-comment (doall (line-seq r))))))

(defn  html?
  [item]
  (and (.exists item) (.isFile item) (.endsWith (.toLowerCase (.getName item)) ".html")))

(defn not-empty?
  [target]
  (not(empty? target )))

(defn -main
  [x]
  (clojure.pprint/pprint
    (filter not-empty?
            (map extract-imart-comment-from-file
                 (filter html?
                         (file-seq (clojure.java.io/file x)))))))
