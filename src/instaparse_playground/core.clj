(ns instaparse-playground.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]))

(defn -main
  "Demonstrating code formatting and syntax transformation"
  [& args]
                                                 ;; Lets create an example of a program in two languages
  (let [clojurishProgram "(println ( + 1300 30 7))"         ;; The first language looks like Lisp and is called Clojurish
        pythyProgram "println(1300     + 30 + 7)"]     ;; The second language follows a more python-style syntax and is called Pythy

    ;; Next, we define a separate parser for both Clojurish and Pythy    
    (def clojurishParser
      (insta/parser
        "
        PROGRAM = EXPR (<SPACE>+ EXPR)*
        <EXPR> = NUMBER | STRING | SEXP
        <SEXP> = <lparen> SPACE* FN SPACE* <rparen>
        <FN> = ADD | PRINT
        ADD = <'add' | '+'> (SPACE+ NUMBER)+
        PRINT = <'println'> SPACE+ EXPR
        NUMBER = #'\\d+'
        STRING = #'\\\".+?\\\"'
        <SPACE> = <#'\\s+'>
        <lparen> = <'('>
        <rparen> = <')'>

       "))

    (def pythyParser 
      (insta/parser
        "

        PROGRAM = EXPR (<SPACE>+ EXPR)*
        <EXPR> = (NUMBER | STRING | FNINV | OPINV)
        FNINV = FN <lparen> (EXPR SPACE*)* <rparen>
        OPINV = EXPR SPACE+ OP SPACE+ EXPR

        <OP> = ADD
        FN = PRINT

        <ADD> = '+'
        <PRINT> = 'println'

        STRING = #'\".*?\"'
        NUMBER = #'\\d+'
        <SPACE> = <#'\\s+'>
        <lparen> = <'('>
        <rparen> = <')'>

        "))

    (def clojurishTranspiler 
      {
       :PRINT (fn [args] (str "(println " args ")"))
       :NUMBER (fn [arg] (str arg))
       :ADD (fn [& args] (str "(+ " (clojure.string/join " " args) ")"))
       :PROGRAM (fn [& args] clojure.string/join "\n" args)
       })

    (def pythyTranspiler
    {
      :FNINV (fn [fnname & args] (str fnname "(" (clojure.string/join ", " args) ")"))
      :NUMBER (fn [arg] (str arg))
      :OPINV (fn [& args] (clojure.string/join " " args))
      :FN (fn [fname] fname)
      :PRINT (fn [& args] "println")
      :PROGRAM (fn [& args] clojure.string/join "\n" args)
      })

    (def eval-options
      {
       :PROGRAM (fn [& args] 1)
       :NUMBER (comp edn/read-string str)
       :ADD +
       :PRINT println
       :FNINV (fn [fname & args] (apply (resolve (symbol fname)) args))
       :FN (fn [fname] fname)
       :OPINV (fn [arg1 op arg2] (apply (resolve (symbol op)) arg1 arg2 []))
      })

    (println "\n\n")
    (println (str "Clojurish Program: " "\n\t\t\t" clojurishProgram))
    (println (str "AST:\n\t\t\t" (clojurishParser clojurishProgram)))
    (print "Formatted:\n\t\t\t" )
    (println (first (insta/transform clojurishTranspiler (clojurishParser clojurishProgram))))
    (defn clojurishEvaler [input]
      (->> (clojurishParser input) (insta/transform eval-options)))
    (print "Result:\n\t\t\t")
    (clojurishEvaler clojurishProgram)

    (println "\n\n")

    (println (str "Pythy Program" "\n\t\t\t" pythyProgram))
    (println (str "AST\n\t\t\t" (pythyParser pythyProgram)))
    (print "Formatted:\n\t\t\t")
    (println (first (insta/transform pythyTranspiler (pythyParser pythyProgram))))
    (defn pythyEvaler [input]
      (->> (pythyParser input) (insta/transform eval-options)))
    (print "Result:\n\t\t\t")
    (pythyEvaler pythyProgram)

    (println "\n\n\n")
    (println (clojurishParser "(println 2)\n(println 3)"))
    (clojurishEvaler "(println 2)\n(println 3)")
    (println (pythyParser "println(12)\nprintln(13)"))
    (pythyEvaler "println(12)\nprintln(13)")
    (println "\n\n\n")
    ;; Finally, convert a Clojurish program into Pythy!
  )
)
