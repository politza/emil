;; -*- lexical-binding: t -*-

;; Copyright (C) 2024  Andreas Politz

;; Author: Andreas Politz

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
