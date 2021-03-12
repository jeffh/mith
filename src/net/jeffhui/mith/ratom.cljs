(ns net.jeffhui.mith.ratom)

;; Loosely based off of reagent's ratoms

(def ^:private ^:dynamic *ratom-context*)

(defn- defer-action [f]
  (if js/requestAnimationFrame
    (js/requestAnimationFrame (fn [_] (f)))
    (js/setTimeout f 16)))

(defonce ^:private react-queue nil)
(declare flush!)
(defn- rx-queue [ref]
  (when (nil? react-queue)
    (set! react-queue #js[])
    (defer-action flush!))
  (.push react-queue ref))

(defprotocol ^:private IReactive
  (_update [this]))

(defn- capturing-deref [^clj sink f]
  (set! (.-captured sink) nil)
  (let [res (binding [*ratom-context* sink]
              (f))]
    (set! (.-dirty sink) false)
    (doseq [ref (.-captured sink)]
      (when (satisfies? IReactive ref)
        (rx-queue ref)))
    res))

(defn- notify-deref [ref]
  (when-some [^clj sink *ratom-context*]
    (let [c (.-captured sink)]
      (if (nil? c)
        (set! (.-captured sink) (array ref))
        (.push c ref)))))

(deftype RAtom [at]
  IAtom

  IWatchable
  (-add-watch [_ key f] (-add-watch at key f))
  (-notify-watches [_ old new] (-notify-watches at old new))
  (-remove-watch [_ key] (-remove-watch at key))

  IMeta
  (-meta [_] (meta at))

  IWithMeta
  (-with-meta [_ new-meta] (RAtom. (-with-meta at new-meta)))

  ISwap
  (-swap! [_ f] (swap! at f))
  (-swap! [_ f x] (swap! at f x))
  (-swap! [_ f x y] (swap! at f x y))
  (-swap! [_ f x y more] (apply swap! at f x y more))

  IReset
  (-reset! [_ new-value] (reset! at new-value))

  IDeref
  (-deref [this]
    (notify-deref this)
    @at)

  IEquiv
  (-equiv [o other] (identical? o other))

  IHash
  (-hash [this] (goog/getUid this)))

(deftype Reaction [act at ^:mutate ^boolean dirty]
  IAtom

  IReactive
  (_update [_]
    (let [old @at
          new (try (act)
                   (catch :default e
                     (js/console.error "error in reaction" e)))]
      (when (or (not (identical? old new))
                dirty)
        (reset! at new))))

  IWatchable
  (-add-watch [_ key f] (-add-watch at key f))
  (-notify-watches [_ old new] (-notify-watches at old new))
  (-remove-watch [_ key] (-remove-watch at key))

  IMeta
  (-meta [_] (meta at))

  IWithMeta
  (-with-meta [_ new-meta] (Reaction. act (-with-meta at new-meta) dirty))

  IDeref
  (-deref [this]
    (notify-deref this)
    (when (or dirty (nil? dirty))
      (reset! at (act))
      (when-not (nil? dirty)
        (set! (.-dirty this) false)))
    @at)

  IEquiv
  (-equiv [o other] (identical? o other))

  IHash
  (-hash [this] (goog/getUid this)))

(defn ratom [value]
  (->RAtom (atom value)))

(defn reaction [f]
  (->Reaction f (atom nil) true))

(defn run [f]
  (->Reaction f (atom nil) nil))

(defn poll [interval f]
  (let [a (ratom nil)]
    (js/setInterval #(reset! a (f)) interval)
    a))

(defn run-in-reaction [f derefs key reaction]
  (let [^clj r #js{}
        res (capturing-deref r f)]
    (when (.-captured r)
      (swap! derefs into (.-captured r))
      (doseq [ref (.-captured r)]
        (add-watch ref key (fn [_ _ old new]
                             (when (not= old new)
                               (reaction))))))
    res))
(defn flush! []
  (loop []
    (let [q react-queue]
      (when-not (nil? q)
        (set! react-queue nil)
        (dotimes [i (alength q)]
          (let [^Reaction r (aget q i)]
            (_update r)))
        (recur)))))