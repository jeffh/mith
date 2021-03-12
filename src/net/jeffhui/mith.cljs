(ns net.jeffhui.mith
  (:require-macros [net.jeffhui.mith])
  (:require [net.jeffhui.mith.ratom :as ratom]
            ["mithril" :as m]))

(defn suppress-redraw!
  "Given an event from inside an event handler, suppress mithril's default redraw behavior."
  [evt]
  (set! (.-redraw evt) false))

(defn redraw!
  "Forcefully trigger mithril to rerender all components."
  []
  (ratom/flush!)
  (m/redraw))

(defn mount
  "Render a component c to DOM node root. Will automatically re-render when
  component indicates it needs re-rendering.
   
  Passing nil as c performs an unmount."
  [root c]
  (m/mount root c))

(defn render
  "Renders a component or virtual DOM nodes to DOM to DOM root. Will not
  automaticallyre-render component."
  [root view]
  (ratom/flush!)
  (m/render root view))

(defn trust
  "Does not attempt to escape the given string when used inside virtual DOM.
  Leads to code injection attacks if the input isn't from a trusted source."
  [raw-html]
  (m/trust raw-html))

(defn fragment
  "Attach mithril attributes to a collection of virtual dom nodes/components."
  [attrs children]
  (m/fragment (clj->js attrs) (into-array children)))

(defn route
  "Configures routes to given components.

   (route (js/document.getElementById \"root\")
          \"/home\"
          {\"/home\"     HomeComponent
           \"/page/:id\" PageComponent})
   "
  ([root routes] (m/route root (clj->js routes)))
  ([root default-route routes] (m/route root default-route (clj->js routes))))

(def skip-route
  "Alias to m.route.SKIP, which is a sentinel value to indicate a route should skip."
  m/route.SKIP)

(defn redirect
  "Redirects to a given route, or default route if the given path is not
  defined. Asynchronously renders mount points."
  ([path] (m/route.set path))
  ([path params] (m/route.set path (clj->js params)))
  ([path params options] (m/route.set path (clj->js params) (clj->js options))))

(defn current-route
  "Returns the last fully resolved route path, without any prefixes.
 
  May be different from what's displayed in the location bar while an asynchronous route is pending."
  [] (m/route.get))

(defn route-prefix
  "Returns the router prefix. The router prefix is a fragment of the URL that
  dictates the underlying strategy used by the router.
   
  s one of the following:
   - \"#!\" (default): Using the fragment identifier (aka the hash) portion
            of the URL. A URL using this strategy typically looks like
            https://localhost/#!/page1
   - \"?\": Using the querystring. A URL using this strategy typically looks like https://localhost/?/page1
   - \"\": Using the pathname. A URL using this strategy typically looks like https://localhost/page1
   "
  [] m/route.prefix)
(defn set-route-prefix!
  "Defines a router prefix. The router prefix is a fragment of the URL that
  dictates the underlying strategy used by the router.
   
  Can be set to one of the following:
   - \"#!\" (default): Using the fragment identifier (aka the hash) portion
            of the URL. A URL using this strategy typically looks like
            https://localhost/#!/page1
   - \"?\": Using the querystring. A URL using this strategy typically looks like https://localhost/?/page1
   - \"\": Using the pathname. A URL using this strategy typically looks like https://localhost/page1
  "
  [value] (set! m/route.prefix value))

(defn parse-query-string
  "Turns a string in the form of '?a=1&b=2' to a clojure map"
  [query] (js->clj (m/parseQueryString query)))

(defn build-query-string
  "Turns a clojure map into a string of form 'a=1&b=2'"
  [m] (m/buildQueryString (clj->js m)))

(defn build-path
  "Turns a path template and a parameters map into a stirng of form '/path/user?a=1&b2' "
  ([query] (m/buildPathname query))
  ([query params] (m/buildPathname query (clj->js params))))

(defn parse-path
  "Turns a string of the form '/path/user?a=1&b2' into a clojure map"
  ([value] (clj->js (m/parsePath value))))

(defn element
  ([el] (m el))
  ([el attrs] (m el (clj->js attrs)))
  ([el attrs child & body] (m el (clj->js attrs) (to-array (into [child] body)))))

(def ^:dynamic *warn-on-interpret* false)

(defn interpret-element
  "Do not use. Reserved for internal use only.
   
   Perhaps will be implemented some day."
  [form]
  (when ^boolean *warn-on-interpret*
    (js/console.warn "possible interpretation of form" (pr-str form)))
  form)

(defn request
  "Makes an XHR/AJAX requests, and returns a promise"
  [{:keys [method url credentials? data user password timeout
           config-fn headers type serialize deserialize extract
           body?]
    :or   {method :get
           type   identity}}]
  ;; returns a promise
  (m/request #js{:method          (.toUpperCase (name method))
                 :url             url
                 :withCredentials credentials?
                 :data            data
                 :headers         (clj->js headers)
                 :user            user
                 :password        password
                 :timeout         timeout
                 :config          config-fn
                 :type            type
                 :serialize       serialize
                 :deserialize     deserialize
                 :extract         extract
                 :useBody         body?}))

(defn- reactive-view [view-fn dirty-ratom]
  (fn [& args]
    (ratom/run-in-reaction #(apply view-fn args) view-fn #(do (reset! dirty-ratom true)
                                                               (m/redraw)))))

(defn component
  "Creates a component that supports reactive atoms to determine if re-rendering is needed.
   
   opts can either be the view function, or a map of mithril methods for a component to implement.

   "
  [opts]
  (let [dirty (atom true)
        onbeforeupdate (fn [_]
                         (let [d @dirty]
                           (when d (reset! dirty false))
                           d))]
    (if (fn? opts)
      #js{:view           (reactive-view opts dirty)
          :onbeforeupdate onbeforeupdate}
      #js{:view           (reactive-view (:view opts) dirty)
          :oninit         (:init opts)
          :oncreate       (:oncreate opts)
          :onbeforeupdate (or (:onbeforeupdate opts) onbeforeupdate)
          :onupdate       (:onupdate opts)
          :onbeforeremove (:onbeforeupdate opts)
          :onremove       (:onremove opts)})))