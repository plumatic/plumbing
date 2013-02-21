(ns plumbing.resource
  "Experimental.
   
   Library for working with (graphs of) resources, which are values that 
   additionally support an optional (close x) and (handlers x) method 
   that respectively shutdown the resource, or return a map of http handlers
   under which the resource should be exposed."
  (:require
   [plumbing.core :as plumbing]
   [plumbing.map :as map]
   [plumbing.fnk.pfnk :as pfnk]))


(defprotocol PCloseable
  "Protocol for closeable resources, which need to be shut down when you're
   done with them (i.e., thread pools or http servers)"
  (close [this]))

(extend-protocol PCloseable
  java.io.Closeable
  (close [this] (.close ^java.io.Closeable this))
  
  Object
  (close [this])
  nil
  (close [this]))


(defprotocol PServable
  "Protocol for servable resources, which expose a map of HTTP handlers for
   remote interaction."
  (handlers [this]))

(extend-protocol PServable
  Object
  (handlers [this] {})
  nil 
  (handlers [this] {}))


(defn- resource-wrap [instantiated-atom-key keyseq node]
  (pfnk/fn->fnk
   (fn [m]
     (let [r (node m)]
       (swap! (instantiated-atom-key m) conj [keyseq r])
       r))
   [(assoc (pfnk/input-schema node)  
      instantiated-atom-key true)
    (pfnk/output-schema node)]))

(defn resource-transform [instantiated-atom-key g]
  (assert (not (contains? g instantiated-atom-key)))
  (assoc (map/map-leaves-and-path (partial resource-wrap instantiated-atom-key) g)
    instantiated-atom-key (plumbing/fnk [] (atom []))))

(defn force-walk 
  "Ensure the (presumably lazy) graph instance inst has its nodes instantiated
   in the same order as graph-spec (from which is was produced).
   Easier than making sure that all transforms preserve order, when you care about it."
  [graph-spec graph-inst]
  (map/map-leaves-and-path
   (fn [keyseq node]
     (log/infof "Starting %s" keyseq)
     (get-in graph-inst keyseq))
   graph-spec)
  graph-inst)

(defn handlers-map [instantiated-resources]
  (map/unflatten   
   (for [[keyseq res] instantiated-resources
         :let [h (handlers res)]
         :when (seq h)]
     [keyseq h])))

(defn shutdown! [instantiated-resources]
  (doseq [[keyseq res] (reverse instantiated-resources)]
    (try (log/infof "Shutting down %s" keyseq)
         (close res)
      (catch Throwable t (log/errorf t "Error shutting down %s" keyseq)))))

