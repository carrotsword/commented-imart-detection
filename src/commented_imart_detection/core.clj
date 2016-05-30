(ns commented-imart-detection.core
  (:use [clojure.pprint]
        [spyscope.core]))

(defn line-stator
  [line-and-number]
  (let [line (last line-and-number)
        line-number (first line-and-number)
        start (.indexOf line "<!--")
        end (.indexOf line "-->")
        line-state (cond
                     (= start end -1) :continue
                     (= start -1) :end
                     (= end -1) :start
                     (< start end) :start-end
                     (< end start) :end-start )]
    {:start start
     :end (+ 3 end)
     :state line-state
     :line line
     :line-number line-number}))


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

;;; start, end, continue, start/end, end/start, off

(def initial-result { :comment ""
                      :comments []
                      :line-number 0
                      :state :off })

(defn comment-extractor
  [current line-state]
  (let [new-status (transit (:state current) (:state line-state))
        line (:line line-state)
        start (:start line-state)
        end (:end line-state)
        comments (:comments current)]
    (case  new-status
      :off current
      :on        (assoc current
                   :comment (str (:comment current) line ))
      :start     (assoc current
                   :comment (subs line start)
                   :line-number (:line-number line-state))
      :end       (assoc current
                   :comment ""
                   :comments (conj comments [(:line-number current) (str (:comment current) (subs line 0 end))]))
      :start-end (assoc current
                   :comments (conj comments [(:line-number line-state)(subs line start end)]))
      :end-start (assoc current
                   :comment (subs line start)
                   :comments (conj comments [(:line-number current)(str (:comment current) (subs line 0 end))])
                   :line-number (:line-number line-state))
      initial-result )))

(defn contains-imart?
  [line-comment]
  (let [string (last line-comment)]
    (or (<= 0 (.indexOf string "<imart"))
        (<= 0 (.indexOf string "</imart"))
        (<= 0 (.indexOf string "imart>")))))

(defn detector
  [file]
  (with-open [r (clojure.java.io/reader file)]
    (let [extract-result (reduce comment-extractor initial-result (map line-stator (map vector (iterate inc 0) (doall (line-seq r)))))
          comments (:comments extract-result)]
      (map (fn [item] {:file (.getAbsolutePath file)
              :line-number (first item)
              :comment (last item)})
           (filter contains-imart? comments)))))

(defn html-filter
  [item]
  (and (.exists item) (.isFile item) (.endsWith (.toLowerCase (.getName item)) ".html")))

(defn not-empty?
  [target]
  (not(empty? target )))

(defn -main
  [x]
  (clojure.pprint/pprint (filter not-empty? (map detector (filter html-filter (file-seq (clojure.java.io/file x)))))))
