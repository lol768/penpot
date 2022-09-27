;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.shapes
  "A workspace specific shapes wrappers.

  Shapes that has some peculiarities are defined in its own
  namespace under app.ui.workspace.shapes.* prefix, all the
  others are defined using a generic wrapper implemented in
  common."
  (:require
   [app.main.data.workspace.state-helpers :as wsh]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.constraints :as gsc]
   [app.common.geom.point :as gpt]
   [app.main.store :as st]
   [app.common.uuid :as uuid]
   [app.common.geom.shapes.intersect :as gsi]
   [cuerdas.core :as str]
   
   [app.common.pages.helpers :as cph]
   [app.main.ui.context :as ctx]
   [app.main.ui.shapes.circle :as circle]
   [app.main.ui.shapes.image :as image]
   [app.main.ui.shapes.rect :as rect]
   [app.main.ui.shapes.text.fontfaces :as ff]
   [app.main.ui.workspace.shapes.bool :as bool]
   [app.main.ui.workspace.shapes.common :as common]
   [app.main.ui.workspace.shapes.frame :as frame]
   [app.main.ui.workspace.shapes.group :as group]
   [app.main.ui.workspace.shapes.path :as path]
   [app.main.ui.workspace.shapes.svg-raw :as svg-raw]
   [app.main.ui.workspace.shapes.text :as text]
   [app.util.object :as obj]
   [rumext.alpha :as mf]))

(declare shape-wrapper)
(declare group-wrapper)
(declare svg-raw-wrapper)
(declare bool-wrapper)
(declare root-frame-wrapper)
(declare nested-frame-wrapper)

(def circle-wrapper (common/generic-wrapper-factory circle/circle-shape))
(def image-wrapper (common/generic-wrapper-factory image/image-shape))
(def rect-wrapper (common/generic-wrapper-factory rect/rect-shape))

(mf/defc root-shape
  "Draws the root shape of the viewport and recursively all the shapes"
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false}
  [props]
  (let [objects       (obj/get props "objects")
        active-frames (obj/get props "active-frames")
        shapes        (cph/get-immediate-children objects)

        ;; We group the objects together per frame-id so if an object of a different
        ;; frame changes won't affect the rendering frame
        frame-objects
        (mf/use-memo
         (mf/deps objects)
         #(cph/objects-by-frame objects))]

    [:& (mf/provider ctx/active-frames) {:value active-frames}
     ;; Render font faces only for shapes that are part of the root
     ;; frame but don't belongs to any other frame.
     (let [xform (comp
                  (remove cph/frame-shape?)
                  (mapcat #(cph/get-children-with-self objects (:id %))))]
       [:& ff/fontfaces-style {:shapes (into [] xform shapes)}])

     (for [shape shapes]
       [:*
        
        
        (cond
          (not (cph/frame-shape? shape))
          [:& shape-wrapper
           {:shape shape
            :key (:id shape)}]

          (cph/root-frame? shape)
          [:& root-frame-wrapper
           {:shape shape
            :key (:id shape)
            :objects (get frame-objects (:id shape))
            :thumbnail? (not (contains? active-frames (:id shape)))}]

          :else
          [:& nested-frame-wrapper
           {:shape shape
            :key (:id shape)
            :objects (get frame-objects (:id shape))}])

        [:rect {:x (-> shape :selrect :x)
                :y (-> shape :selrect :y)
                :width (-> shape :selrect :width)
                :height (-> shape :selrect :height)
                :style {:fill "none" :stroke "red" :stroke-width 1}}]

        

        ])]))

(mf/defc shape-wrapper
  {::mf/wrap [#(mf/memo' % (mf/check-props ["shape"]))]
   ::mf/wrap-props false}
  [props]
  (let [shape (obj/get props "shape")

        active-frames
        (when (cph/root-frame? shape) (mf/use-ctx ctx/active-frames))

        thumbnail?
        (and (some? active-frames)
             (not (contains? active-frames (:id shape))))

        opts  #js {:shape shape :thumbnail? thumbnail?}]
    [:*
     
     (when (and (some? shape) (not (:hidden shape)))
       (case (:type shape)
         :path    [:> path/path-wrapper opts]
         :text    [:> text/text-wrapper opts]
         :group   [:> group-wrapper opts]
         :rect    [:> rect-wrapper opts]
         :image   [:> image-wrapper opts]
         :circle  [:> circle-wrapper opts]
         :svg-raw [:> svg-raw-wrapper opts]
         :bool    [:> bool-wrapper opts]

         ;; Only used when drawing a new frame.
         :frame [:> nested-frame-wrapper opts]

         nil))

     (let [objects (wsh/lookup-page-objects @st/state)
           parent (get objects (:parent-id shape))
           child shape

           points
           (-> child
               :points
               (gsh/transform-points (:transform-inverse parent))
               (gsh/points->rect)
               (gsh/rect->points) ;; Restore to points so we can transform them
               (gsh/transform-points (:transform parent)))]

       
       [:polyline {:points (->> (concat points [(nth points 0)]) (map #(str (:x %) "," (:y %))) (str/join " "))
                   :style {:fill "none" :stroke "red" :stroke-width 1}}]
       #_[:rect {:x (-> rect-before :x)
               :y (-> rect-before :y)
               :width (-> rect-before :width)
               :height (-> rect-before :height)
               :style {:fill "none" :stroke "red" :stroke-width 1}}])

     (when (and (some? (:parent-id shape)) (not= uuid/zero (:parent-id shape)))
       (let [objects (wsh/lookup-page-objects @st/state)
             parent (get objects (:parent-id shape))

             child shape
             
             parent-points-before (:points parent)
             child-points-before (gsc/bounding-box-parent-transform child parent)

             [p0 p1 p2 p3] child-points-before

             r (gsc/right-vector child-points-before parent-points-before)
             l (gsc/left-vector child-points-before parent-points-before)
             t (gsc/top-vector child-points-before parent-points-before)
             b (gsc/bottom-vector child-points-before parent-points-before)

             c1 (gsc/center-horizontal-vector child-points-before parent-points-before)
             c2 (gsc/center-vertical-vector child-points-before parent-points-before)


             pt (gpt/add p0 t)
             pr (gpt/add p1 r)
             pb (gpt/add p2 b)
             pl (gpt/add p3 l)

             pc1 (gpt/add p1 c1)
             pc2 (gpt/add p1 c2) 
             ]

         [:*

          [:g
           [:circle {:r 5
                     :cx (:x pc1)
                     :cy (:y pc1)
                     :style {:stroke "green"}}]
           
           [:line {:x1 (:x p1)
                   :y1 (:y p1)
                   :x2 (:x pc1)
                   :y2 (:y pc1)
                   :style {:stroke "green"}}]]

          [:g
           [:circle {:r 5
                     :cx (:x pc2)
                     :cy (:y pc2)
                     :style {:stroke "green"}}]
           
           [:line {:x1 (:x p1)
                   :y1 (:y p1)
                   :x2 (:x pc2)
                   :y2 (:y pc2)
                   :style {:stroke "green"}}]]

          [:g
           [:circle {:r 5
                     :cx (:x pt)
                     :cy (:y pt)
                     :style {:stroke "blue"}}]
           
           [:line {:x1 (:x p0)
                   :y1 (:y p0)
                   :x2 (:x pt)
                   :y2 (:y pt)
                   :style {:stroke "blue"}}]]

          [:g
           [:circle {:r 5
                     :cx (:x pr)
                     :cy (:y pr)
                     :style {:stroke "blue"}}]

           [:circle {:r 5
                     :cx (:x p1)
                     :cy (:y p1)
                     :style {:stroke "blue"}}]
           
           [:line {:x1 (:x p1)
                   :y1 (:y p1)
                   :x2 (:x pr)
                   :y2 (:y pr)
                   :style {:stroke "blue"}}]]

          [:g
           [:circle {:r 5
                     :cx (:x pb)
                     :cy (:y pb)
                     :style {:stroke "blue"}}]
           
           [:line {:x1 (:x p2)
                   :y1 (:y p2)
                   :x2 (:x pb)
                   :y2 (:y pb)
                   :style {:stroke "blue"}}]]

          [:g
           [:circle {:r 5
                     :cx (:x pl)
                     :cy (:y pl)
                     :style {:stroke "blue"}}]
           
           [:line {:x1 (:x p3)
                   :y1 (:y p3)
                   :x2 (:x pl)
                   :y2 (:y pl)
                   :style {:stroke "blue"}}]]

          
          
          
          

          ]))
     ]))

(def group-wrapper (group/group-wrapper-factory shape-wrapper))
(def svg-raw-wrapper (svg-raw/svg-raw-wrapper-factory shape-wrapper))
(def bool-wrapper (bool/bool-wrapper-factory shape-wrapper))
(def root-frame-wrapper (frame/root-frame-wrapper-factory shape-wrapper))
(def nested-frame-wrapper (frame/nested-frame-wrapper-factory shape-wrapper))

