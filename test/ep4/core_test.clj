(ns ep4.core-test
  (:require [clojure.test :refer :all]
            [ep4.core :refer :all]))

(deftest aux-functions
  (testing "Helper functions"
    (is (= true (ep4.core/terminal? "a")))
    (is (= false (ep4.core/terminal? "A")))
    (is (= true (ep4.core/non-terminal? "A")))
    (is (= false (ep4.core/non-terminal? "a")))
    (is (= "ȸ" (ep4.core/generate-latin-non-terminal "a")))
    (is (= "ɮ" (ep4.core/generate-latin-non-terminal "z")))
    (is (= "α" (ep4.core/get-greek-non-terminal 0)))
    (is (= "ϼ" (ep4.core/get-greek-non-terminal 25)))
    (is (= nil (ep4.core/get-greek-non-terminal 100)))
    (is (= ["abc" "bcd" "cde" "def" "efg" "fgh"] (ep4.core/group-substrings-by "abcdefgh" 3)))
    (is (= ["a"] (ep4.core/group-substrings-by "a" 3)))
    (is (= [["a" "bcde"] ["ab" "cde"] ["abc" "de"] ["abcd" "e"]] (ep4.core/gen-all-splits "abcde")))
    (is (= [["a" "b"]] (ep4.core/gen-all-splits "ab")))
    (is (= [["a" ""]] (ep4.core/gen-all-splits "a")))))

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

(deftest unit-test
  (testing "UNIT transformation"
    (is (= [["S" "a"] ["A" "a"] ["$" "a"]] (ep4.core/UNIT [["S" "A"] ["A" "a"] ["$" "S"]])))
    (is (= [["$" "d"] ["A" "d"] ["B" "d"] ["C" "d"]] (ep4.core/UNIT [["$" "A"] ["A" "B"] ["B" "C"] ["C" "d"]])))))

(deftest full-test
  (testing "Full transformation"
    (is (=
         [["ȸ" "a"] ["ȹ" "b"] ["α" "Sȹ"] ["α" "b"] ["S" "ȸα"] ["$" "ȸα"] ["$" ""]]
         (ep4.core/FULL [["S" "aSb"] ["S" ""]] "S")))
    (is (=
         [["ȸ" "a"] ["α" "Sȸ"] ["S" "ȸα"] ["ȹ" "b"] ["β" "Sȹ"] ["S" "ȹβ"] ["$" "ȸα"] ["$" "ȹβ"]]
         (ep4.core/FULL [["S" "aSa"] ["S" "bSb"]] "S")))))

(deftest CNF-verification
  (testing "Chomsky Normal Form validation"
    (is (= true (ep4.core/rules-follow-CNF? (ep4.core/FULL [["S" "aSb"] ["S" ""]] "S"))))
    (is (= true (ep4.core/rules-follow-CNF? (ep4.core/FULL [["S" "aSa"] ["S" "bSb"] ["S" ""]] "S"))))
    (is (= true (ep4.core/rules-follow-CNF? (ep4.core/FULL [["A" "aA"] ["A" "abc"]] "A"))))
    (is (= true (ep4.core/rules-follow-CNF? (ep4.core/FULL [["S" "a"] ["S" "aS"]] "S"))))))

(deftest CYK-test
  (testing "CYK algorithm"
    (is (= true (ep4.core/CYK "aabb" (ep4.core/FULL [["S" "aSb"] ["S" ""]] "S"))))
    (is (= true (ep4.core/CYK "aaabbb" (ep4.core/FULL [["S" "aSb"] ["S" ""]] "S"))))
    (is (= true (ep4.core/CYK "abaabbbaabbaabbbaaba" (ep4.core/FULL [["S" "aSa"] ["S" "bSb"] ["S" ""]] "S"))))
    (is (= true (ep4.core/CYK "aaaaaaaaaaaaaabc" (ep4.core/FULL [["A" "aA"] ["A" "abc"]] "A"))))
    (is (= true (ep4.core/CYK "a" (ep4.core/FULL [["S" "a"] ["S" "aS"]] "S"))))
    (is (= false (ep4.core/CYK "" (ep4.core/FULL [["S" "a"] ["S" "aS"]] "S"))))
    (is (= true (ep4.core/CYK "" (ep4.core/FULL [["S" ""] ["S" "aS"]] "S"))))))