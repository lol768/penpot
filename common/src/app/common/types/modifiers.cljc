;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.types.modifiers
  (:require
   [app.common.data :as d]
   [app.common.spec :as us]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [clojure.spec.alpha :as s]))

(defn add-move
  ([object x y]
   (add-move object (gpt/point x y)))

  ([object vector]
   (assoc-in
    object
    [:modifiers :displacement]
    (gmt/translate-matrix (:x vector) (:y vector)))))

(defn add-resize
  [object vector origin]
  (-> object
      (assoc-in [:modifiers :resize-vector] vector)
      (assoc-in [:modifiers :resize-origin] origin)))
