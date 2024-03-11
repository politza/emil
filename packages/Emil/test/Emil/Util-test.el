;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'dash)
(require 'Emil/Util)

(describe "Emil:Util"
  (describe "Emil:Util:NameGenerator"
    :var (subject)

    (before-each (setq subject (Emil:Util:NameGenerator)))

    (it "starts with single letters"
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'a)
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'b)
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'c))

    (it "then moves to two letters"
      (--iterate (prog1 it (Emil:Util:NameGenerator:next it))
                 subject 27)
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'aa)
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'bb)
      (expect (Emil:Util:NameGenerator:next subject)
              :to-equal 'cc))))
