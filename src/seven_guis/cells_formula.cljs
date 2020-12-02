(ns seven-guis.cells-formula
  (:require [seven-guis.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))


(defn parse-number [src]
  (let [edn (util/read-edn-if-valid src)]
    (when (number? edn)
      edn)))


(def formula-fns
  {"sum" +
   "mul" *})


(def token-regexes
  [[:coord #"^[A-Z]\d{1,2}(?!\w)"]
   [:number #"^-?\d+(\.\d+)?(?!\w)"]
   [:symbol #"^\w+"]
   [:open-paren #"^\("]
   [:comma #"^,"]
   [:close-paren #"^\)"]
   [:colon #"^:"]
   [:whitespace #"^\s+"]])


(defn pop-token [src]
  (reduce (fn [_ [k regex]]
            (let [match (when-let [x (re-find regex src)]
                          (if (vector? x)
                            (first x)
                            x))]
              (when match
                (reduced [{:type k
                           :src match}
                          (subs src (count match))]))))
          nil
          token-regexes))


(comment
  (pop-token "")
  (pop-token "|")
  (pop-token "1")
  (pop-token "abc(1,2.3)")
  (pop-token (second *1)))


(defn tokenize [src]
  (loop [tokens []
         src src]
    (if (empty? src)
      tokens
      (let [[{:keys [type] :as token}
             rest
             :as result]
            (pop-token src)]
        (when result
          (recur
           (cond-> tokens
             (not= type :whitespace)
             (conj token))
           rest))))))


(comment
  (tokenize "abc(1,A0,mul(-2.3, B3))")
  (tokenize "abc(1,A0,||????|||mul(-2.3, B3))"))


(defn match-types? [tokens types]
  (= (take (count types) (map :type tokens))
     types))


(defn watch-range [a b] :XXX)


(defn pop-ast
  [tokens]
  (cond

    (match-types? tokens [:number])
    [(first tokens) (rest tokens)]

    (match-types? tokens [:coord :colon :coord])
    (let [[start _ end & rest] tokens
          watches (watch-range (:src start) (:src end))]
      [{:type :range :watches watches}
       rest])

    (match-types? tokens [:coord])
    [(first tokens) (rest tokens)]

    (match-types? tokens [:symbol :open-paren])
    (let [f (:src (first tokens))]
      (loop [args []
             tokens (drop 2 tokens)]
        (if (match-types? tokens [:close-paren])
          [{:type :call :f f :args args}
           (rest tokens)]
          (let [[arg tokens] (pop-ast tokens)]
            (if (:error arg)
              arg
              (recur (conj args arg)
                     (cond-> tokens
                       (match-types? tokens [:comma]) rest)))))))
    :else
    [{:error (str "Unexpected token: " (:src (first tokens)))}]))

(comment
  (pop-ast (tokenize "sum(1, mul(A1:B2,2), neg(C3)) 5")))


(defn ast
  "Return the abstract syntax tree of the expression in the tokens, or error."
  [tokens]
  (let [[{:keys [error] :as ast} leftovers] (pop-ast tokens)]
    (cond
      error ast
      (seq leftovers) {:error (apply str "Unexpected extra input: " (map :src leftovers))}
      :else ast)))

(comment
  (ast (tokenize "sum(1, mul(A1:B2,2), neg(C3))"))
  (ast (tokenize "sum(1, mul(A1:B2,2), neg(C3)) 5")))

(defn compile [ast]
  (if (:error ast)
    ast
    :XXX))


(comment
  (ast (tokenize "sum(1, mul(A1:B2,2), neg(C3)) 5")))

(defn parse-formula [src]
  (some-> src tokenize ast compile)
  #_(when (str/starts-with? src "=")
    (parse-node (subs src 1))))

(defn parse [src]
  (or (parse-formula src)
      {:f (constantly (or (parse-number src) src))}))
