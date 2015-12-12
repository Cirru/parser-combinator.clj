
(ns cirru.parser-combinator.tree)

(declare resolve-dollar)
(declare resolve-comma)

(defn turn-vec [xs]
  (mapv
    (fn [x]
      (if (seq? x) (turn-vec x) x))
    xs))

; helper since rest returns list
(defn restv [xs]
  (into [] (rest xs)))
(defn concatv [xs ys]
  (into [] (concat xs ys)))

(defn dollar-helper [before after]
  (if (= (count after) 0) before
    (let
      [ cursor (first after)
        cursor-rest (restv after)]
      (cond
        (vector? cursor) (dollar-helper
          (conj before (resolve-dollar cursor))
          cursor-rest)
        (= (first after) "$") (conj before
          (resolve-dollar cursor-rest))
        :else (dollar-helper
          (conj before cursor)
          cursor-rest)))))

(defn resolve-dollar [xs]
  (if (= (count xs) 0) xs
    (dollar-helper [] xs)))

(defn comma-helper [before after]
  (if (= (count after) 0) before
    (let
      [ cursor (first after)
        cursor-rest (restv after)]
      (if
        (and (vector? cursor) (> (count cursor) 0))
        (let
          [ head (first cursor)]
          (cond
            (vector? head) (comma-helper
              (conj before (resolve-comma cursor))
              cursor-rest)
            (= head ",") (comma-helper before
              (concatv
                (resolve-comma (restv cursor))
                cursor-rest))
            :else (comma-helper
              (conj before (resolve-comma cursor))
              cursor-rest)))
        (comma-helper (conj before cursor) cursor-rest)))))

(defn resolve-comma [xs]
  (if (= (count xs) 0) xs
    (comma-helper [] xs)))
