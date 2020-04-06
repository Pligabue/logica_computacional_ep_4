(ns ep4.core-test
  (:require [clojure.test :refer :all]
            [ep4.core :refer :all]))

(deftest a-test
  (testing "Belongs to grammar - Return value"
    (is (= true (ep4.core/belongs-to-grammar "aaabbb" ["S" [["S" "ab"] ["S" "aSb"]]])))))

(deftest b-test
  (testing "Apply rule - Return value"
    (is (= "aaSbb" (ep4.core/apply-rule "aSb" ["S" "aSb"])))))

(deftest c-test
  (testing "Apply rules to bases - Return value"
    (is
     (=
      ["ab" "aSb" nil "aabb" "aaSbb" nil nil nil "ab"]
      (ep4.core/apply-rules-to-bases ["S" "aSb" "aB"] [["S" "ab"] ["S" "aSb"] ["B" "b"]])))))

(deftest chomsky-rule 
  (testing "Rule is in the Chomsky Normal Form"
    (is (= true (ep4.core/verify-chomsky-normal-form-rule ["A" "BC"] "S")))
    (is (= true (ep4.core/verify-chomsky-normal-form-rule ["A" "a"] "S")))
    (is (= true (ep4.core/verify-chomsky-normal-form-rule ["S" ""] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" "SB"] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" "BS"] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" ""] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" "aa"] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" "aC"] "S")))
    (is (= false (ep4.core/verify-chomsky-normal-form-rule ["A" "Ba"] "S")))
    (is (thrown? Exception (ep4.core/verify-chomsky-normal-form-rule ["AA" "BC"] "S")))))

(deftest start-test
  (testing "START transformation"
    (is (= [["S" "AB"] ["A" "a"] ["B" "b"] ["$" "S"]] (ep4.core/START [["S" "AB"] ["A" "a"] ["B" "b"]] "S")))))

(deftest term-test
  (testing "TERM transformation"
    (is (= [["ȼ" "c"] ["ȿ" "e"] ["ɀ" "g"] ["A" "BȼDȿFɀH"] ["B" "ȼȿɀ"]] (ep4.core/TERM [["A" "BcDeFgH"] ["B" "ceg"]])))
    (is (= [["ȸ" "a"] ["A" "ȸȸȸȸȸ"] ["ȹ" "b"] ["B" "ȸȹ"]] (ep4.core/TERM [["A" "aaaaa"] ["B" "ab"]])))))

(deftest bin-test
  (testing "BIN transformation"
    (is (= [["α" "EF"] ["β" "Dα"] ["γ" "Cβ"] ["A" "Bγ"] ["δ" "IO"] ["ζ" "Uδ"] ["η" "Yζ"] ["B" "Tη"]] (ep4.core/BIN [["A" "BCDEF"] ["B" "TYUIO"]])))
    (is (= [["α" "BC"] ["S" "Aα"] ["β" "BA"] ["B" "Aβ"]] (ep4.core/BIN [["S" "ABC"] ["B" "ABA"]])))))

(deftest del-test
  (testing "DEL transformation"
    (is (= 
         [["$" "AbB"] ["$" "bB"] ["$" "Ab"] ["$" "b"] ["$" "C"] ["B" "AA"] ["B" "A"] ["B" "AC"] ["B" "C"] ["C" "b"] ["C" "c"] ["A" "a"]]
         (ep4.core/DEL [["$" "AbB"] ["$" "C"] ["B" "AA"] ["B" "AC"] ["C" "b"] ["C" "c"] ["A" "a"] ["A" ""]])))
    (is (= [["$" "S"] ["$" ""]] (ep4.core/DEL [["$" "S"] ["S" ""]])))))