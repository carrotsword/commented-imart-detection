(ns commented-imart-detection.core
  (:use [clojure.pprint]
        [spyscope.core]))

(defrecord NumberedLine [number data])

(defn type-n [data] {:data data :type :normal})

(defn type-c [data] {:data data :type :comment})

(defn is-n? [t] (= :normal (:type t)))

(defn is-c? [t] (= :comment (:type t)))

(defn parse
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

(defn parse-line
  [prev-status line]
  (let [initial-data (if (= prev-status :comment) (type-c "") (type-n ""))]
    (reduce parse [initial-data] line )))

(defn analyse-line
  [parsed-lines nl]
  (let [parsed (parse-line (:end-status (last parsed-lines)) (:data nl))]
  (conj parsed-lines {:data parsed
                      :line-number (:number nl)
                      :end-status (if (= (:type (last parsed)) :normal) :normal :comment )})))

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

(defn flatten-parsed [coll parsed]
  (->> (:data parsed)
       (map (fn [data] (assoc data :line-number (:line-number parsed))))
       (concat coll)))

(defn extract-html-comment
  [lseq]
  (->> (add-line-number lseq)
       (reduce analyse-line [])
       (reduce flatten-parsed [])
       (filter is-c?)))

(defn extract-imart-comment
  [lseq]
    (filter contains-imart? (extract-html-comment lseq)))

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
