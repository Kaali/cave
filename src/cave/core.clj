(ns cave.core)

(def *fill-probability* 0.4)

(defrecord GenParam [r1-cutoff r2-cutoff reps])

(defrecord Cave [rows cols data])

(defn get-point [c x y]
  (nth (:data c) (+ x (* y (:cols c)))))

(defn rand-point []
  (if (< *fill-probability* (rand)) :floor :wall))

(defn at-boundary-fn [rows cols]
  (let [rows (dec rows)
        cols (dec cols)]
    (fn [x y] (or (= y 0) (= y rows)
                  (= x 0) (= x cols)))))

(defn init-cave [rows cols]
  (let [at-boundary (at-boundary-fn rows cols)
        data (for [y (range rows)
                   x (range cols)]
               (if (at-boundary x y)
                 :wall
                 (rand-point)))]
    (Cave. rows cols data)))

(defn print-point [p] (print (if (= p :wall) \# \.)))

(defn print-cave [c]
  (dotimes [y (:rows c)]
    (dotimes [x (:cols c)]
      (print-point (get-point c x y)))
    (println)))

(defn out-of-cave [c x y]
  (or (< y 0) (>= y (:rows c))
      (< x 0) (>= x (:cols c))))

(defn around [c x y r]
  (letfn [(it [point] (range (- point r) (+ 1 point r)))]
    (for [xx (it x)
          yy (it y)
          :when (not (out-of-cave c xx yy))]
      (get-point c xx yy))))

(defn adjacency [c x y]
  (letfn [(count-walls [data] (count (filter #(= :wall %) data)))
          (adj [r] (count-walls (around c x y r)))]
    [(adj 1) (adj 2)]))

(defn generation [c {:keys [r1-cutoff r2-cutoff]}]
  (let [{:keys [rows cols]} c
        at-boundary (at-boundary-fn rows cols)
        data (for [y (range rows)
                   x (range cols)]
               (if (at-boundary x y)
                 :wall
                 (let [[adj-r1 adj-r2] (adjacency c x y)]
                   (if (or (>= adj-r1 r1-cutoff)
                           (<= adj-r2 r2-cutoff))
                     :wall
                     :floor))))]
    (Cave. rows cols data)))

(defn evolve [c param]
  (nth (iterate #(generation % param) c) (dec (:reps param))))

(defn generate-cave [rows cols & params]
  (let [c (init-cave rows cols)]
    (reduce (fn [c param] (evolve c param)) c params)))

(comment
  (print-cave (generate-cave 20 20
                             (GenParam. 6 1 2)
                             (GenParam. 5 1 3))))