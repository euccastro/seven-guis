(ns seven-guis.cells-formula
  (:require [seven-guis.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn]))


(defn cell-key [col row]
  (assert (and (string? col) (integer? row))
          "got these swapped?")
  (str col row))


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

;; XXX: use clojure.core/halt-when instead?
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
  (into []
        (comp (map first) ; collect tokens, ignore remaining sources
              (bail-with #(= (:type %) :error))
              (take-while #(not= (:type %) :eof))
              (remove #(= (:type %) :whitespace)))
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
    (let [[start _ end & rest] tokens]
      [{:type :range :start (:src start) :end (:src end)}
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
  (pop-ast (tokenize "A1"))
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


(defmulti compile
  "AST node -> compiled formula or error (see docstring for `parse`)"
  :type)


(defmethod compile :default
  [x]
  (assert false, (str "Unknown AST element: " (pr-str x)) ))


(defmethod compile :error [{:keys [msg]}]
  {:type :error  ; make recognizable so it can bubble up
   :watches #{}
   :f (constantly msg)})


(defmethod compile :number [{:keys [src]}]
  {:watches #{}
   :f (constantly (edn/read-string src))})


(defmethod compile :coord [{:keys [src]}]
  {:watches #{src}
   :f #(get % src)})


(defn watch-range [start end]
  (let [[start-number end-number] (->> [start end]
                                       (map #(subs % 1))
                                       (map edn/read-string))]
    (for [row (range start-number (inc end-number))
          col (util/char-range start end)]
      (cell-key col row))))

(comment

  (subs "A1" 1)
  (def start "A1")
  (def end "B3")
  (watch-range "A11" "B1")
  ;; => ()
  ;; but I won't reverse them to catch this case because order is possibly
  ;; significant (e.g., when calling sub)
  )

(defmethod compile :range [{:keys [start end]}]
  (let [watches (watch-range start end)]
    (if (seq watches)
      {:watches (set watches)
       :f #(map % watches)}
      ;; disable this since it's most likely a puzzling typo
      (compile {:type :error :msg "TYPE ERROR: empty range"}))))


(def builtins
  {"sum" +
   "sub" -
   "mul" *
   "div" /})


(defn fuzzy-cat
  "Transducer that `cat`s seqable elements only"
  [rf]
  ;; In production code I'd copy the definition of `preserving-reduced` here
  ;; instead.
  (let [rf1 (#'cljs.core/preserving-reduced rf)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([accum input]
       (if (seqable? input)
         (reduce rf1 accum input)
         (rf accum input))))))

(comment
  (into [1 2] fuzzy-cat [3 [4] [5 [6]]]) ;; => [1 2 3 4 5 [6]]
  )


(defmethod compile :call
  [{:keys [f args]}]
  (let [compiled-args (map compile args)
        error (first (filter #(= (:type %) :error)
                             compiled-args))]
    (or error
        {:watches
         (apply set/union (map :watches compiled-args))
         :f
         (fn [watch-m]
           (transduce (comp (map #((:f %) watch-m))
                            fuzzy-cat)
                      (builtins f)
                      compiled-args))})))


(defn check-top-level-range [{:keys [type] :as ast}]
  (if (= type :range)
    {:type :error :msg "SYNTAX ERROR: cannot use range on its own as a formula"}
    ast))

(defn parse-formula
  [src]
  (when (str/starts-with? src "=")
    (-> src (subs 1) tokenize ast check-top-level-range compile)))

(comment

  ((juxt) 5)

  (defn test-parse-formula [src]
    ((-> (parse-formula (str "=" src)) :f) {"A1" 1 "A2" 2}))

  (-> "A1:A2" tokenize ast)
  (test-parse-formula "1")
  (test-parse-formula "A1")
  (test-parse-formula "A1:A2")
  (test-parse-formula "sum()")
  (test-parse-formula "sum(2)")
  (test-parse-formula "sum(2, 3)")
  (test-parse-formula "sum(A1,2)")
  (test-parse-formula "sum(A1,A2)")
  (test-parse-formula "sum(A1:A2)")
  (test-parse-formula "sum(1, A1:A2)")
  ;; nested errors bubble up
  (test-parse-formula "sum(5, sub(2, A2:A1))")
)


(defn parse
  "return a compiled formula, of the form
  {:watches #{cell-id...} :f ([watch-m] -> number)}
  or {:error msg} if this looks like a broken formula."
  [src]
  (or (parse-formula src)
      {:watches #{}
       :f (constantly (or (parse-number src) src))}))
