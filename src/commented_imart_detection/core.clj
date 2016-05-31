(ns commented-imart-detection.core
  (:use [clojure.pprint]
        [spyscope.core]))

(defrecord NumberedLine [number data])

(defrecord ModifiedLine [start end state data line-number])

(defn line-stator
  [numbered-line]
  (let [line (:data numbered-line)
        start (.indexOf line "<!--")
        end (.indexOf line "-->")
        line-state (cond
                     (= start end -1) :continue
                     (= start -1) :end
                     (= end -1) :start
                     (< start end) :start-end
                     (< end start) :end-start )]
    (ModifiedLine.
      start
      (if (= end -1) -1 (+ 3 end))
      line-state
      line
      (:number numbered-line))))

(defn modify-lines
  [lseq]
  (map line-stator (map (fn [a b] (NumberedLine. a b)) (iterate inc 0) lseq)))


(defn transit
  [current new-state]
  (case current
    :off ( case new-state
           :continue :off
           :end :off
           :end-start :start
           new-state )
    :end ( case new-state
           :continue :off
           :end-start :start
           new-state)
    :start (case new-state
             :continue :on
             :start-end :end
             new-state )
    :start-end (case new-state
                 :continue :off
                 :end :off
                 :end-start :start
                 new-state)
    :end-start (case new-state
                 :continue :on
                 :start :on
                 :start-end :end
                 new-state)
    :on (case new-state
          :continue :on
          :start :on
          :start-end :end
          new-state)
    (case new-state
      :continue :off
      :end :off
      :end-start :start
      new-state)))

;;; start, end, continue, start/end, end/start, off, on

(def initial-result { :comment ""
                      :comments []
                      :line-number 0
                      :state :off })

(defn conj-nl
  [comments line data]
  (conj comments (NumberedLine. line data)))

(defn collect-comments
  [current modified-line]
  (let [line (:data modified-line)
        start (:start modified-line)
        end (:end modified-line)
        cmt (:comment current)
        comments (:comments current)
        current-no (:line-number current)
        newline-no (:line-number modified-line)
        new-state (transit (:state current) (:state modified-line))]
    (assoc (case  new-state
             :off       current
             :on        (assoc current
                          :comment (str cmt line ))
             :start     (assoc current
                          :comment (subs line start)
                          :line-number newline-no)
             :end       (assoc current
                          :comment ""
                          :comments (conj-nl comments current-no (str cmt (subs line 0 end))))
             :start-end (assoc current
                          :comments (conj-nl comments newline-no (subs line start end)))
             :end-start (assoc current
                          :comment (subs line start)
                          :comments (conj-nl comments current-no (str cmt (subs line 0 end)))
                          :line-number newline-no)
             initial-result )
      :state new-state)))

(defrecord Result [file numbered-line])

(defn contains-imart?
  [numbered-line]
  (let [string (.toLowerCase (:data numbered-line))]
    (or (<= 0 (.indexOf string "<imart"))
        (<= 0 (.indexOf string "</imart"))
        (<= 0 (.indexOf string "imart>")))))

(defn extract-html-comment
  [lseq]
  (:comments (reduce collect-comments initial-result (modify-lines lseq))))

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
