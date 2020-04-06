(ns ep4.core
  (:gen-class))

(require '[clojure.string :as string])

(defn is-upper-case [word]
  (and (= (string/upper-case word) word) (not (string/blank? word))))

(defn verify-chomsky-normal-form-rule [rule starting-char]
  (let [left-side (first rule) 
        left-size (count left-side) 
        right-side (second rule) 
        right-size (count right-side)]
    (if (or (not= left-size 1) (not (is-upper-case left-side))) 
      (throw (Exception. "Not a context-free language"))
      (case right-size
        0 (if (= left-side starting-char) true false)
        1 (if (is-upper-case right-side) false true )
        2 (if (and 
               (is-upper-case (subs right-side 0 1))
               (is-upper-case (subs right-side 1 2))
               (not= starting-char (subs right-side 0 1))
               (not= starting-char (subs right-side 1 2)))
            true
            false)
        false))))


(comment "Assumindo que caracteres não terminais são letras maiúsculas e terminais são letras minúsculas")

(comment "Funções para a transformação START")

(defn get-start-rule [starting-char]
  ["$" starting-char])

(defn START [rules starting-char]
  (conj rules (get-start-rule starting-char)))

(comment "Funções para a transformação TERM")

(def alphabet ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"])

(def latin-letters ["ȸ" "ȹ" "ȼ" "Ƚ" "ȿ" "ɀ" "ɀ" "Ƀ" "Ʉ" "Ʌ" "Ɇ" "Ɉ" "Ɋ" "Ɍ" "Ɏ" "ɐ" "ɓ" "ɕ" "ɖ" "ɚ" "ɞ" "ɟ" "ɠ" "ɣ" "ɧ" "ɮ"])

(defn generate-latin-non-terminal [terminal]
  (let [index (.indexOf alphabet (string/upper-case terminal))]
    (get latin-letters index)))

(defn rule-term-transformation 
  ([rule] (rule-term-transformation rule 0 []))
  ([rule i acc]
   (let [left-side (first rule) 
         right-side (second rule) 
         current-char (str (get right-side i))
         non-terminal (generate-latin-non-terminal current-char)]
     (cond
       (string/blank? current-char) (conj acc rule)
       (is-upper-case current-char) (recur rule (inc i) acc)
       (not (is-upper-case current-char)) (recur [left-side (string/replace right-side current-char non-terminal)] (inc i) (conj acc [non-terminal current-char]))
       :else false))))

(defn TERM [rules]
  (vec (distinct (reduce concat (map (fn [rule] (rule-term-transformation rule)) rules)))))

(comment "Funções para a transformação BIN")

(def greek-letters ["α" "β" "γ" "δ" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "π" "ρ" "ς" "σ" "τ" "υ" "φ" "χ" "ψ" "ω" "ϸ" "ϻ" "ϼ"])

(defn get-greek-non-terminal [index]
  (get greek-letters index))

(defn rule-bin-transformation 
  ([rule i] (rule-bin-transformation rule i []))
  ([rule i acc]
   (let [left-side (first rule) 
         right-side (second rule)
         right-size (count right-side)
         last-two (subs right-side (- right-size 2))
         all-but-last-two (subs right-side 0 (- right-size 2))
         non-terminal (get-greek-non-terminal i)]
     (if (> right-size 2)
       (recur [left-side (str all-but-last-two non-terminal)] (inc i) (conj acc [non-terminal last-two]))
       (conj acc rule)))))

(rule-bin-transformation ["A" "BCDEF"] 0)

(defn BIN 
  ([rules] (BIN rules 0 [] 0))
  ([rules i acc non-terminal-i]
   (let [rule (get rules i)
         right-side (second rule)
         right-size (count right-side)
         next-i (- right-size 2)]
     (cond 
       (nil? rule) acc
       (>= right-size 2) (recur rules (inc i) (vec (concat acc (rule-bin-transformation rule non-terminal-i))) next-i)
       :else (recur rules (inc i) (conj acc rule) non-terminal-i)))))

(BIN [["A" "A"]])

(comment "Funções para a transformação DEL")

(defn all-chars-in-vector
  ([sample ref] (all-chars-in-vector sample ref 0))
  ([sample ref i]
   (let [current-char (str (get sample i))]
     (if (= i (count sample))
       true
       (if (some (partial = current-char) ref)
         (recur sample ref (inc i))
         false)))))

(defn get-nullables
  ([rules] (get-nullables rules 0 [] []))
  ([rules i acc last-iter]
   (let [rule (get rules i)
         number-of-rules (count rules)
         left-side (first rule)
         right-side (second rule)
         right-size (count right-side)]
     (cond
       (>= i number-of-rules) (if (= acc last-iter)
                           acc
                           (recur rules 0 acc acc))
       (zero? right-size) (recur rules (inc i) (vec (distinct (conj acc left-side))) last-iter)
       (> right-size 0) (if (all-chars-in-vector right-side acc)
                          (recur rules (inc i) (vec (distinct (conj acc left-side))) last-iter)
                          (recur rules (inc i) acc last-iter))
       :else (throw (Exception. "Right size can't be negative."))))))

(defn pseudo-remove-char [index base]
  (print (type base))
  (str (subs base 0 index) "_" (subs base (inc index))))

(defn remove-nullables-from-rule
  ([rule nullables] (remove-nullables-from-rule rule nullables 0 [(second rule)])) 
  ([rule nullables i acc]
   (let [right-side (second rule)
         current-char (str (get right-side i))
         nullable-char? (all-chars-in-vector current-char nullables)]
     (if (empty? current-char)
       (vec (map (fn [x] [(first rule) x]) (vec (map (fn [x] (string/replace x "_" "")) acc))))
       (if nullable-char?
         (recur
          rule
          nullables
          (inc i)
          (vec (concat acc (map (fn [x] (pseudo-remove-char i x)) acc))))
         (recur 
          rule
          nullables
          (inc i)
          acc))))))

(defn DEL 
  ([rules] (DEL rules 0 []))
  ([rules i acc]
   (let [rule (get rules i)
         nullables (get-nullables rules)]
     (if rule
       (recur rules (inc i) (vec (concat acc (remove-nullables-from-rule rule nullables))))
       (vec (remove (fn [x] (and (not= (first x) "$") (empty? (second x)))) (vec (distinct acc))))))))


(comment "Funções para a transformação UNIT")

(defn is-non-terminal [sample]
  (boolean (re-matches #"[^a-z]" sample)))

(defn is-unit-rule [rule]
  (and (= 1 (count (second rule))) (is-non-terminal (second rule))))

(defn get-unit-transformations [main-rule rules]
  (vec (remove nil? 
          (map (fn [rule] 
                 (if (= (second main-rule) (first rule)) 
                   [(first main-rule) (second rule)]
                   nil)) 
               rules))))

(defn UNIT [rules]
  (vec (reduce concat (map 
                       (fn [rule] 
                         (if (is-unit-rule rule)
                           (get-unit-transformations rule rules)
                           [rule]))
                       rules))))

(UNIT 
 (DEL 
  (BIN 
   (TERM 
    (START [["S" "aSb"] ["S" ""]] "S")))))