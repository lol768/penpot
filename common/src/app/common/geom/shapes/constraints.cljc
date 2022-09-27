;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.geom.shapes.constraints
  (:require
   [app.common.geom.shapes.intersect :as gsi]
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.common :as gco]
   [app.common.geom.shapes.rect :as gre]
   [app.common.geom.shapes.transforms :as gst]
   [app.common.math :as mth]
   [app.common.uuid :as uuid]))

;; Auxiliary methods to work in an specifica axis
(defn other-axis [axis]
  (if (= :x axis) :y :x))

(defn get-delta-start [axis rect tr-rect]
  (if (= :x axis)
    (- (:x1 tr-rect) (:x1 rect))
    (- (:y1 tr-rect) (:y1 rect))))

(defn get-delta-end [axis rect tr-rect]
  (if (= :x axis)
    (- (:x2 tr-rect) (:x2 rect))
    (- (:y2 tr-rect) (:y2 rect))))

(defn get-delta-size [axis rect tr-rect]
  (if (= :x axis)
    (- (:width tr-rect) (:width rect))
    (- (:height tr-rect) (:height rect))))

(defn get-delta-scale [axis rect tr-rect]
  (if (= :x axis)
    (/ (:width tr-rect) (:width rect))
    (/ (:height tr-rect) (:height rect))))

(defn get-delta-center [axis center tr-center]
  (if (= :x axis)
    (- (:x tr-center) (:x center))
    (- (:y tr-center) (:y center))))

(defn get-displacement
  ([axis delta]
   (get-displacement axis delta 0 0))

  ([axis delta init-x init-y]
   (if (= :x axis)
     (gpt/point (+ init-x delta) init-y)
     (gpt/point init-x (+ init-y delta)))))

(defn get-scale [axis scale]
  (if (= :x axis)
    (gpt/point scale 1)
    (gpt/point 1 scale)))

(defn get-size [axis rect]
  (if (= :x axis)
    (:width rect)
    (:height rect)))

;; Constraint function definitions

(defmulti constraint-modifier (fn [type & _] type))

(defmethod constraint-modifier :start
  [_ axis parent _ _ transformed-parent]

  []
  #_(let [transformed-parent-rect (:selrect transformed-parent)
        parent-rect (:selrect parent)
        delta-start (get-delta-start axis parent-rect transformed-parent-rect)]
    (if-not (mth/almost-zero? delta-start)
      [{:type :move :vector (get-displacement axis delta-start)}]
      [])))

(defmethod constraint-modifier :end
  [_ axis parent _ _ transformed-parent]
  (let [transformed-parent-rect (:selrect transformed-parent)
        parent-rect (:selrect parent)
        delta-end (get-delta-end axis parent-rect transformed-parent-rect)]
    (if-not (mth/almost-zero? delta-end)
      [{:type :move :vector (get-displacement axis delta-end)}]
      [])))

(defmethod constraint-modifier :fixed
  [_ axis parent child _ transformed-parent]
  (let [transformed-parent-rect (:selrect transformed-parent)
        parent-rect (:selrect parent)
        child-rect (gre/points->rect (:points child))

        delta-start  (get-delta-start axis parent-rect transformed-parent-rect)
        delta-size    (get-delta-size axis parent-rect transformed-parent-rect)
        child-size   (get-size axis child-rect)]
    (if (or (not (mth/almost-zero? delta-start))
            (not (mth/almost-zero? delta-size)))
      [{:type :move
        :vector (get-displacement axis delta-start)}
       {:type :resize
        :origin (get-displacement axis delta-start (:x child-rect) (:y child-rect))
        :vector (get-scale axis (/ (+ child-size delta-size) child-size))}]
      [])))

(defmethod constraint-modifier :center
  [_ axis parent _ _ transformed-parent]
  (let [transformed-parent-rect (:selrect transformed-parent)
        parent-rect (:selrect parent)
        parent-center (gco/center-rect parent-rect)
        transformed-parent-center (gco/center-rect transformed-parent-rect)
        delta-center (get-delta-center axis parent-center transformed-parent-center)]
    (if-not (mth/almost-zero? delta-center)
      [{:type :move
        :vector (get-displacement axis delta-center)}]
      [])))

(defmethod constraint-modifier :scale
  [_ axis parent child modifier transformed-parent]

  [(assoc-in modifier [:vector (other-axis axis)] 1)]

  #_(let [transformed-parent-rect (:selrect transformed-parent)
        parent-rect (:selrect parent)
        delta-start  (get-delta-start axis parent-rect transformed-parent-rect)
        delta-scale   (get-delta-scale axis parent-rect transformed-parent-rect)

        
        trans-diff (gmt/multiply (:transform transformed-parent (gmt/matrix))
                                 (:transform-inverse parent (gmt/matrix)))
        ]

    

    #_(prn "!!" trans-diff)
    #_(prn "??delta-start" axis delta-start)
    #_(prn "  >" (get-displacement axis delta-start))
    #_(prn "  >" (-> (get-displacement axis delta-start)
                   (gpt/transform (:transform transformed-parent) )))

    (if (or (not (mth/almost-zero? delta-start))
            (not (mth/almost-zero? delta-scale)))
      [
       #_{:type :transform
        :transform trans-diff

        
        }
       #_{:type :move
        :vector (-> (get-displacement axis delta-start)
                    (gpt/transform (:transform transformed-parent) )

                    )}

       #_{:type :resize
        :transform (:transform parent)
        :transform-inverse (:transform-inverse parent)
        :origin (gpt/point (:x transformed-parent-rect) (:y transformed-parent-rect))
        :vector (get-scale axis delta-scale)}]
      [])))

(defmethod constraint-modifier :default [_ _ _ _ _]
  [])

(def const->type+axis
  {:left :start
   :top :start
   :right :end
   :bottom :end
   :leftright :fixed
   :topbottom :fixed
   :center :center
   :scale :scale})

(defn default-constraints-h
  [shape]
  (if (= (:parent-id shape) uuid/zero)
    nil
    (if (= (:parent-id shape) (:frame-id shape))
      :left
      :scale)))

(defn default-constraints-v
  [shape]
  (if (= (:parent-id shape) uuid/zero)
    nil
    (if (= (:parent-id shape) (:frame-id shape))
      :top
      :scale)))

#_(defn clean-modifiers
  "Remove redundant modifiers"
  [{:keys [displacement resize-vector resize-vector-2] :as modifiers}]

  (cond-> modifiers
    ;; Displacement with value 0. We don't move in any direction
    (and (some? displacement)
         (mth/almost-zero? (:e displacement))
         (mth/almost-zero? (:f displacement)))
    (dissoc :displacement)

    ;; Resize with value very close to 1 means no resize
    (and (some? resize-vector)
         (mth/almost-zero? (- 1.0 (:x resize-vector)))
         (mth/almost-zero? (- 1.0 (:y resize-vector))))
    (dissoc :resize-origin :resize-vector)

    (and (some? resize-vector)
         (mth/almost-zero? (- 1.0 (:x resize-vector-2)))
         (mth/almost-zero? (- 1.0 (:y resize-vector-2))))
    (dissoc :resize-origin-2 :resize-vector-2)))


(defn right-vector
  [child parent]
  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)
        dir-v (gpt/to-vec p0 p1)
        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p1 p2)]
    (gpt/to-vec c1 cp)))


(defn left-vector
  [child parent]

  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)
        dir-v (gpt/to-vec p0 p1)
        cp (gsi/line-line-intersect c3 (gpt/add c3 dir-v) p0 p3)]
    (gpt/to-vec c3 cp)))

(defn top-vector
  [child parent]

  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)
        dir-v (gpt/to-vec p0 p3)
        cp (gsi/line-line-intersect c0 (gpt/add c0 dir-v) p0 p1)]
    (gpt/to-vec c0 cp)))

(defn bottom-vector
  [child parent]

  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)
        dir-v (gpt/to-vec p0 p3)
        cp (gsi/line-line-intersect c2 (gpt/add c2 dir-v) p2 p3)]
    (gpt/to-vec c2 cp)))

(defn center-horizontal-vector
  [child parent]

  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)

        dir-v (gpt/to-vec p0 p1)

        p1c (gpt/add p0 (gpt/scale dir-v 0.5))
        p2c (gpt/add p3 (gpt/scale dir-v 0.5))
        
        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p1c p2c)]
    
    (gpt/to-vec c1 cp)))

(defn center-vertical-vector
  [child parent]
  (let [[p0 p1 p2 p3] (-> parent :points)
        [c0 c1 c2 c3] (-> child :points)

        dir-v (gpt/to-vec p1 p2)

        p3c (gpt/add p0 (gpt/scale dir-v 0.5))
        p2c (gpt/add p1 (gpt/scale dir-v 0.5))
        
        cp (gsi/line-line-intersect c1 (gpt/add c1 dir-v) p3c p2c)]
    
    (gpt/to-vec c1 cp)))

(defn calc-child-modifiers
  [parent child modifiers ignore-constraints transformed-parent]

  (let [constraints-h
        (if-not ignore-constraints
          (:constraints-h child (default-constraints-h child))
          :scale)

        constraints-v
        (if-not ignore-constraints
          (:constraints-v child (default-constraints-v child))
          :scale)

        process-operation
        (fn [modifier]
          (cond
            (and (or (not= :scale constraints-h) (not= :scale constraints-v))(= :resize (:type modifier)))
            ;; We check the constraints only when resizes and one of the constraints is not scale
            (let [modifiers-h (constraint-modifier (constraints-h const->type+axis) :x parent child modifier transformed-parent)
                  modifiers-v (constraint-modifier (constraints-v const->type+axis) :y parent child modifier transformed-parent)]
              (d/concat-vec modifiers-h modifiers-v))

            ;; Otherwise we let the constraints as they were
            :else
            [modifier]))

        tranformed-child
        (-> child (merge {:modifiers modifiers}) gst/transform-shape)


        bb-tr-after-1
        (-> tranformed-child
            :points
            (gco/transform-points (:transform-inverse transformed-parent))
            (gre/points->rect))

        bb-tr-before
        (-> child
            :points
            (gco/transform-points (:transform-inverse parent))
            (gre/points->rect))

        
        ;;_ (prn "!!" (/ (:width bb-tr-after-1) (:width bb-tr-before)))

        modifiers
        (-> modifiers
            (update :v2 conj
                    ;; This resize will leave the shape in its original position relative to the parent
                    {:type :resize
                     :transform (:transform transformed-parent)
                     :transform-inverse (:transform-inverse transformed-parent)
                     :origin (-> transformed-parent :points (nth 0))
                     :vector (gpt/point
                              (/ (:width bb-tr-before) (:width bb-tr-after-1))
                              (/ (:height bb-tr-before) (:height bb-tr-after-1)))}))

        tranformed-child-2
        (-> child (merge {:modifiers modifiers}) gst/transform-shape)

        bb-tr-after-2
        (-> tranformed-child-2
            :points
            (gco/transform-points (:transform-inverse transformed-parent))
            (gre/points->selrect))


        right-before (right-vector child parent)
        right-after  (right-vector tranformed-child-2 transformed-parent)
        left-before (left-vector child parent)
        left-after  (left-vector tranformed-child-2 transformed-parent)
        top-before (top-vector child parent)
        top-after  (top-vector tranformed-child-2 transformed-parent)
        bottom-before (bottom-vector child parent)
        bottom-after  (bottom-vector tranformed-child-2 transformed-parent)

        right-angl (gpt/angle-with-other right-before right-after)
        left-angl (gpt/angle-with-other left-before left-after)
        top-angl (gpt/angle-with-other top-before top-after)
        bottom-angl (gpt/angle-with-other bottom-before bottom-after)

        target-right (if (mth/close? right-angl 180) (- (gpt/length right-before)) (gpt/length right-before))
        target-left (if (mth/close? left-angl 180) (- (gpt/length left-before)) (gpt/length left-before))
        target-top (if (mth/close? top-angl 180) (- (gpt/length top-before)) (gpt/length top-before))
        target-bottom (if (mth/close? bottom-angl 180) (- (gpt/length bottom-before)) (gpt/length bottom-before))


        disp-vector-right (gpt/subtract right-after (gpt/scale (gpt/unit right-after) target-right))
        disp-vector-bottom (gpt/subtract bottom-after (gpt/scale (gpt/unit bottom-after) target-bottom))


        ;; CENTER

        c1-before (center-horizontal-vector child parent)
        c1-after  (center-horizontal-vector tranformed-child-2 transformed-parent)
        c1-angl (gpt/angle-with-other c1-before c1-after)
        target-c1 (if (mth/close? c1-angl 180) (- (gpt/length c1-before)) (gpt/length c1-before))

        c2-before (center-vertical-vector child parent)
        c2-after  (center-vertical-vector tranformed-child-2 transformed-parent)
        c2-angl (gpt/angle-with-other c2-before c2-after)
        target-c2 (if (mth/close? c2-angl 180) (- (gpt/length c2-before)) (gpt/length c2-before))

        disp-vector-c1 (gpt/subtract c1-after (gpt/scale (gpt/unit c1-after) target-c1))
        disp-vector-c2 (gpt/subtract c2-after (gpt/scale (gpt/unit c2-after) target-c2))


        ;; left+right

        width-vec
        (gpt/to-vec (-> tranformed-child-2 :points (nth 0))
                    (-> tranformed-child-2 :points (nth 1)))

        resized-width-vec
        (gpt/to-vec (-> tranformed-child-2 :points (nth 0))
                    (gpt/add (-> tranformed-child-2 :points (nth 1))
                             disp-vector-right))

        origin-resize
        (-> tranformed-child-2
            :points
            (gco/transform-points (:transform-inverse transformed-parent))
            (gre/points->selrect)
            (gpt/point)
            (gpt/transform (:transform transformed-parent)))
        
        _ (prn "??"  (/ (gpt/length resized-width-vec) (gpt/length width-vec)))
        ;;_ (prn "??" target-bottom)
        

        modifiers
        (-> modifiers
            (update :v2 conj
                    ;; This resize will leave the shape in its original position relative to the parent
                    ;; Right
                    #_{:type :move
                       :vector disp-vector-right}

                    ;; Bottom
                    #_{:type :move
                       :vector disp-vector-bottom}

                    ;; Center-h
                    #_{:type :move
                       :vector disp-vector-c1}

                    ;; Center-v
                    #_{:type :move
                       :vector disp-vector-c2}


                    {:type :resize
                     :vector (gpt/point (/ (gpt/length resized-width-vec) (gpt/length width-vec)) 1)
                     ;;:origin (-> tranformed-child-2 :points (nth 0))
                     :origin origin-resize
                     :transform (:transform transformed-parent)
                     :transform-inverse  (:transform-inverse transformed-parent)
                     }


                    ))

        

        ]


    

    modifiers)
  
  
  #_(let [
        result
        (if (and (nil? (:resize-vector modifiers))
                 (nil? (:resize-vector-2 modifiers))
                 (nil? (:v2 modifiers)))
          ;; If we don't have a resize modifier we return the same modifiers
          modifiers
          (let [constraints-h
                (if-not ignore-constraints
                  (:constraints-h child (default-constraints-h child))
                  :scale)

                constraints-v
                (if-not ignore-constraints
                  (:constraints-v child (default-constraints-v child))
                  :scale)

                trans-diff (gmt/multiply (:transform transformed-parent (gmt/matrix))
                                         (:transform-inverse parent (gmt/matrix)))
                
                modifiers-h (constraint-modifier (constraints-h const->type+axis) :x parent child modifiers transformed-parent)
                modifiers-v (constraint-modifier (constraints-v const->type+axis) :y parent child modifiers transformed-parent)]

            {:v2 (d/concat-vec
                  ;;[{:type :transform :transform trans-diff}]
                  modifiers-h
                  modifiers-v)}

            #_(.log js/console (:name child) (clj->js modifiers-h) (clj->js modifiers-v))
            ;; Build final child modifiers. Apply transform again to the result, to get the
            ;; real modifiers that need to be applied to the child, including rotation as needed.
            #_(cond-> {}
              ;;(some? (:displacement-after modifiers))
              ;;(assoc :displacement-after (:displacement-after modifiers))

              (or (contains? modifiers-h :displacement-after)
                  (contains? modifiers-v :displacement-after))
              (assoc :displacement-after (cond-> (gpt/point (get-in modifiers-h [:displacement-after :x] 0)
                                                            (get-in modifiers-v [:displacement-after :y] 0))
                                           :always
                                           (gmt/translate-matrix)))

              (or (contains? modifiers-h :displacement)
                  (contains? modifiers-v :displacement))
              (assoc :displacement (cond-> (gpt/point (get-in modifiers-h [:displacement :x] 0)
                                                      (get-in modifiers-v [:displacement :y] 0))
                                     (some? (:resize-transform modifiers))
                                     (gpt/transform (:resize-transform modifiers))

                                     :always
                                     (gmt/translate-matrix)))

              (:resize-vector modifiers-h)
              (assoc :resize-origin (:resize-origin modifiers-h)
                     :resize-vector (gpt/point (get-in modifiers-h [:resize-vector :x] 1)
                                               (get-in modifiers-h [:resize-vector :y] 1)))

              (:resize-vector modifiers-v)
              (assoc :resize-origin-2 (:resize-origin modifiers-v)
                     :resize-vector-2 (gpt/point (get-in modifiers-v [:resize-vector :x] 1)
                                                 (get-in modifiers-v [:resize-vector :y] 1)))

              (:resize-transform modifiers)
              (assoc :resize-transform (:resize-transform modifiers)
                     :resize-transform-inverse (:resize-transform-inverse modifiers))

              :always
              (clean-modifiers))))]

    #_(.log js/console "calc-child-modifiers" (:name parent) (:name child))
    #_(.log js/console "  >" (clj->js modifiers))
    #_(.log js/console "  >" (clj->js (select-keys transformed-parent-rect [:x :y :width :height])))
    #_(.log js/console "  >" (clj->js result))


    result))
