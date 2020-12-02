(ns seven-guis.cells-formula
  (:require [seven-guis.util :as util]
            [clojure.string :as str]
            [clojure.edn :as edn]))


(defn parse-number [src]
  (let [edn (util/read-edn-if-valid src)]
    (when (number? edn)
      edn)))


(def token-regexes
  [[:coord #"^[A-Z]\d{1,2}(?!\w)"]
   [:number #"^-?\d+(\.\d+)?(?!\w)"]
   [:symbol #"^\w+"]
   [:open-paren #"^\("]
   [:comma #"^,"]
   [:close-paren #"^\)"]
   [:colon #"^:"]
   [:whitespace #"^\s+"]])


(defn pop-token
  "return [{:type ... :src ...} remainder-of-src]"
  [src]
  (or
   (when (empty? src) [{:type :eof} ""])
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
           token-regexes)
   [{:type :error
     :msg (str "SYNTAX ERROR: unrecognized token at " (pr-str src))
     :src src} ""]))


(comment
  (pop-token "")
  (pop-token "|")
  (pop-token "1")
  (pop-token "abc(1,2.3)")
  (pop-token (second *1)))


(defn bail-with
  "Transducer that reduces to an item if it matches `f`"
  [f]
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([accum x]
       (if (f x)
         (reduced x)
         (xf accum x))))))


(defn tokenize [src]
  (transduce
   (comp (map first) ; collect tokens, ignore remaining sources
         (bail-with #(= (:type %) :error))
         (take-while #(not= (:type %) :eof))
         (remove #(= (:type %) :whitespace)))
   conj
   []
   (iterate #(pop-token (second %)) (pop-token src))))


(comment
  [nil "abc(1,A0,mul(-2.3, B3))"]
  (pop-token (second *1))
  (tokenize "(1)")
  (tokenize "abc(1,A0,mul(-2.3, B3))")
  (def ret *1)
  (type (last ret))
  (tokenize "abc(1,A0,||????|||mul(-2.3, B3))"))


(defn match-types? [tokens types]
  (= (take (count types) (map :type tokens))
     types))


(defn watch-range [a b]
  ;; XXX
  #{"A1" "B2" "C3"})


(declare pop-ast)


(defn collect-args
  "return [error-msg args remaining-tokens]"
  [tokens]
  (loop [args [] tokens tokens]
    (cond
      (empty? tokens) ["SYNTAX ERROR: unexpected EOF" nil tokens]
      (match-types? tokens [:close-paren]) [nil args (rest tokens)]
      :else
      (let [[arg tokens] (pop-ast tokens)]
        (if (= (:type arg) :error)
          [(:msg arg) nil tokens]
          (recur (conj args arg)
                 (cond-> tokens
                   ;; We allow a trailing comma in the arglist, for no strong
                   ;; reason.
                   (match-types? tokens [:comma]) rest)))))))

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
    (let [f (:src (first tokens))
          [error-msg args remaining-tokens] (collect-args (drop 2 tokens))]
      (if error-msg
        [{:type :error :msg error-msg}]
        [{:type :call :f f :args args} remaining-tokens]))
    :else
    [{:type :error :msg (str "Unexpected token: " (pr-str (:src (first tokens))))}]))


(comment
  (pop-ast (tokenize "sum(1, 2,)"))
  (pop-ast (tokenize "sum(1, mul(2, 3))"))
  (pop-ast (tokenize "sum(1, mul(A1:B2,2), neg(C3)) 5"))
  (pop-ast (tokenize "1 sum(1, mul(A1:B2,2), neg(C3)) 5")))


(defn ast
  "Return the abstract syntax tree of the expression in the tokens, or error."
  [tokens]
  (let [[{:keys [type] :as ast} leftovers] (pop-ast tokens)]
    (cond
      (= type :error) ast  ; this will probably be more useful than the one about leftovers
      (seq leftovers) {:type :error :msg (apply str "Unexpected extra input: " (map :src leftovers))}
      :else ast)))

(comment
  (ast (tokenize "sum(1, mul(A1:B2,2), neg(C3))"))
  ;; missing a closing paren
  (ast (tokenize "sum(1, mul(A1:B2,2, neg(C3))"))
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
