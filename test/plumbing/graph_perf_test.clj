(ns plumbing.graph-perf-test
  "Demo test from Climate"
  (:use
   [plumbing.core :only [fnk]])
  (:require
   [plumbing.graph :as graph]
   [plumbing.fnk.impl :as fnk-impl]
   [plumbing.fnk.pfnk :as pfnk]))

(def ^:const kelvin 273.15)
(def ^:const days-in-year 365)

(defn to-kelvin
  "Converts Celcius to Kelvin"
  ^double [^double temp]
  (+ temp kelvin))

(defn inverse-relational-distance
  "Inverse relational distance between the Earth and sun at a
   given day of the year (1 to 366)"
  ^double [^long day-of-year]
;;  {:pre [(h/between-inclusive day-of-year 1 366)]}
  (-> (* day-of-year 2 Math/PI)
      (/ days-in-year)
      Math/cos
      (* 0.033)
      inc))

(defn solar-declination
  "Solar declination at a given day of the year (1 to 366)"
  ^double [^long day-of-year]
;;  {:pre [(h/between-inclusive day-of-year 1 366)]}
  (-> (* day-of-year Math/PI 2)
      (/ days-in-year)
      (- 1.39)
      Math/sin
      (* 0.409)))

(defn sunset-hour-angle
  "Sunset hour angle given day of year (1 to 366) and latitude in radians"
  ^double [^double solar-dec ^double lat-in-rad]
  ;; {:pre [(h/between-inclusive solar-dec -0.5 0.5)
  ;;        (h/between-inclusive lat-in-rad -3.15 3.15)]
  ;;  :post [(h/between-inclusive % 0 3.15)]}
  (-> (Math/tan lat-in-rad)
      (* (Math/tan solar-dec) -1)
      Math/acos))

(defn sat-vapour-pressure
  "Saturated vapour pressure at given temperature

  Parameters
  ----------
  temp: float
      Temperature, deg C

  Returns
  -------
  Saturated vapour pressure, kPa"
  ^double [^double temp]
  (* 0.6108 (-> (* temp 17.27) (/ (+ temp 237.3)) Math/exp)))

(defn degree-to-radians
  "Convert degrees to radians"
  ^double [^double deg]
  (-> (* deg Math/PI)
      (/ 180)))

(defn solar-rad-et
  "Extraterrestrial solar radiation

   Parameters
   ----------
   day-of-year: int
       Day of the year (1 to 366)
   lat: float
       Latitude, decimal degrees

   Returns:
   -------
   solar radiation, MJ/(m^2*day)"
  ^double [^long day-of-year ^double lat]
  (let [lat-in-rad (degree-to-radians lat)
        inv-dist (inverse-relational-distance day-of-year)
        c (-> (* 24 60 0.0820 inv-dist)
              (/ Math/PI))
        solar-dec (solar-declination day-of-year)
        sun-hour-angle (sunset-hour-angle solar-dec lat-in-rad)
        a  (* sun-hour-angle (Math/sin lat-in-rad) (Math/sin solar-dec))
        b  (* (Math/cos lat-in-rad) (Math/cos solar-dec) (Math/sin sun-hour-angle))]
    (* c (+ a b))))


(def solar-rad-from-temp
  "Estimate the solar radiation from temperature

    Required
    --------
    lat: float,
        Latitude, decimal degrees

    alt: float,
        Altitude, in m

    tmax, tmin: float
        Maximum and minimum temperatures, deg C.  Must be
        between -5 and 45 C.

    doy: int
        Day of the year (1 to 366)

    Optional
    --------
    kRs: float, default = 0.16
        adjustment coefficient for solar radiation

    a: float, default = 0.23
        Canopy reflection coefficient (default is for grass)

    Calculated
    ----------
    tmaxK: float
       Max Temperature in Kelvin

    tminK: float
       Min Temperature in Kelvin

    Ra: float

    Rs: float

    Rso: float

    Rns: float
        Net short wave radiation
    Rn: float
      Net solar radiation, MJ/(m^2*day)"

  {:Ra (fnk [doy lat] (solar-rad-et doy lat))
   :Rs (fnk [tmax tmin {kRs 0.16} Ra] (-> (- tmax tmin) Math/sqrt (* kRs Ra)))
   :Rso (fnk [alt Ra] (-> (* 2e-5 alt) (+ 0.75) (* Ra)))
   :Rns (fnk [{a 0.23} Rs] (-> (- 1 a) (* Rs)))
   :tmaxK (fnk [tmax] (to-kelvin tmax))
   :tminK (fnk [tmin] (to-kelvin tmin))
   :ea (fnk [tmin] (sat-vapour-pressure tmin))
   :term1 (fnk [tmaxK tminK {s 4.903e-9}]
               (-> (Math/pow tmaxK 4) (+ (Math/pow tminK 4)) (* s) (/ 2)))
   :term2 (fnk [ea] (-> (Math/sqrt ea) (* -0.14) (+ 0.34)))
   :term3 (fnk [Rs Rso] (-> (* 1.35 Rs) (/ Rso) (- 0.35)))
   :Rnl (fnk [term1 term2 term3] (* term1 term2 term3))
   :Rn  (fnk [Rns Rnl ](- Rns Rnl))})

(def solar-rad-from-temp-positional
  {:Ra (fnk [doy lat] (plumbing.graph-perf-test/solar-rad-et doy lat))
   :Rs (fnk [tmax tmin {kRs 0.16} Ra] (-> (- tmax tmin) Math/sqrt (* kRs Ra)))
   :Rso (fnk [alt Ra] (-> (* 2e-5 alt) (+ 0.75) (* Ra)))
   :Rns (fnk [{a 0.23} Rs] (-> (- 1 a) (* Rs)))
   :tmaxK (fnk [tmax] (plumbing.graph-perf-test/to-kelvin tmax))
   :tminK (fnk [tmin] (plumbing.graph-perf-test/to-kelvin tmin))
   :ea (fnk [tmin] (plumbing.graph-perf-test/sat-vapour-pressure tmin))
   :term1 (fnk [tmaxK tminK {s 4.903e-9}]
               (-> (Math/pow tmaxK 4) (+ (Math/pow tminK 4)) (* s) (/ 2)))
   :term2 (fnk [ea] (-> (Math/sqrt ea) (* -0.14) (+ 0.34)))
   :term3 (fnk [Rs Rso] (-> (* 1.35 Rs) (/ Rso) (- 0.35)))
   :Rnl (fnk [term1 term2 term3] (* term1 term2 term3))
   :Rn  (fnk [Rns Rnl ](- Rns Rnl))})

(def solar-rad-from-temp-positional-typehinted
  {:Ra (fnk [^long doy ^double lat] (plumbing.graph-perf-test/solar-rad-et doy lat))
   :Rs (fnk [^long tmax ^long tmin {kRs 0.16} Ra] (-> (- tmax tmin) Math/sqrt (* kRs Ra)))
   :Rso (fnk [alt Ra] (-> (* 2e-5 alt) (+ 0.75) (* Ra)))
   :Rns (fnk [{a 0.23} Rs] (-> (- 1 a) (* Rs)))
   :tmaxK (fnk [tmax] (plumbing.graph-perf-test/to-kelvin tmax))
   :tminK (fnk [tmin] (plumbing.graph-perf-test/to-kelvin tmin))
   :ea (fnk [tmin] (plumbing.graph-perf-test/sat-vapour-pressure tmin))
   :term1 (fnk [tmaxK tminK {s 4.903e-9}]
               (-> (Math/pow tmaxK 4) (+ (Math/pow tminK 4)) (* s) (/ 2)))
   :term2 (fnk [ea] (-> (Math/sqrt ea) (* -0.14) (+ 0.34)))
   :term3 (fnk [Rs Rso] (-> (* 1.35 Rs) (/ Rso) (- 0.35)))
   :Rnl (fnk [term1 term2 term3] (* term1 term2 term3))
   :Rn  (fnk [Rns Rnl] (- Rns Rnl))})

(defrecord SolarRadRecord [Ra Rs Rso Rns tmax-kelvin tmin-kelvin ea term-1
                           term-2 term-3 Rnl Rn])
(defn solar-rad-from-temp-fn
  "Estimate the solar radiation from temperature

    Parameters
    ----------
    kRs: float, default = 0.16
        adjustment coefficient for solar radiation

    lat: float,
        Latitude, decimal degrees

    alt: float,
        Altitude, in m

    tmax, tmin: float
        Maximum and minimum temperatures, deg C.  Must be
        between -5 and 45 C.

    day-of-year: int
        Day of the year (1 to 366)

    a: float, default = 0.23
        Canopy reflection coefficient (default is for grass)

    Returns
    -------
    Net solar radiation, MJ/(m^2*day)"
  ([lat alt tmax tmin day-of-year]
     (let [kRs 0.16
           a 0.23]
       (solar-rad-from-temp-fn kRs lat alt tmax tmin day-of-year a)))
  ([kRs lat alt tmax tmin day-of-year]
     (let [a 0.23]
       (solar-rad-from-temp-fn kRs lat alt tmax tmin day-of-year a)))
  ([kRs lat alt tmax tmin day-of-year a]
     (let [Ra (solar-rad-et day-of-year lat)
           Rs (-> (- tmax tmin) Math/sqrt (* kRs Ra)) ;;solar radiaion
           Rso (-> (* 2e-5 alt) (+ 0.75) (* Ra)) ;;Clear sky solar radiation
           Rns (-> (- 1 a) (* Rs)) ;;net short wave radiation
           s 4.903e-9 ;;Stefan-Boltzman constant, MJ/(K^4*m^2*day^-1)
           tmax-kelvin (to-kelvin tmax)
           tmin-kelvin (to-kelvin tmin)
           ea (sat-vapour-pressure tmin)
           term-1 (-> (Math/pow tmax-kelvin 4)
                      (+ (Math/pow tmin-kelvin 4))
                      (* s)
                      (/ 2))
           term-2 (-> (Math/sqrt ea) (* -0.14) (+ 0.34))
           term-3 (-> (* 1.35 Rs) (/ Rso) (- 0.35))
           Rnl (* term-1 term-2 term-3)
           Rn (- Rns Rnl)]
       ;(SolarRadRecord. Ra Rs Rso Rns tmax-kelvin tmin-kelvin ea term-1 term-2
       ;                 term-3 Rnl Rn)
       Rn)))

(defn -main
  [& args]
  ;; simple profiling, comparing the graph implemention
  ;; to the function implementation
  (let [solar-rad-as-graph (time (graph/interpreted-eager-compile solar-rad-from-temp))
        solar-rad-pos-graph (time (graph/eager-compile solar-rad-from-temp-positional))
        solar-rad-pos-graph-pos (time (graph/positional-eager-compile
                                       solar-rad-from-temp-positional
                                       [:lat :alt :tmin :tmax :doy]))
        solar-rad-pos-graph-th (fnk-impl/positional-fn
                                 (graph/eager-compile solar-rad-from-temp-positional-typehinted)
                                 [:lat :alt :tmin :tmax :doy :kRs :a :s])]
    (println "As old eager")
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-as-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205}))))
    (println "As new eager"  (solar-rad-pos-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205}))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-pos-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205}))))
    (println "As new eager positional" (solar-rad-pos-graph-pos 45.0 100.0 15.0 25.0 205))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-pos-graph-pos 45.0 100.0 15.0 25.0 205))))
    (println "As positional-th"  (solar-rad-pos-graph-th 45.0 100.0 15.0 25.0 205  0.16 0.23 4.903e-9))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-pos-graph-th 45.0 100.0 15.0 25.0 205  0.16 0.23 4.903e-9))))
    (println "As fn")
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-from-temp-fn 45.0 100.0 25.0 15.0 205))))))

(comment
  (require '[plumbing.timing :as timing])
  (defn bench []
    (let [solar-rad-as-graph (graph/old-eager-compile solar-rad-from-temp)
          solar-rad-pos-graph (graph/eager-compile
                               solar-rad-from-temp-positional
                               [:lat :alt :tmin :tmax :doy])
          solar-rad-pos-graph-th (graph/eager-compile
                                  solar-rad-from-temp-positional-typehinted
                                  [:lat :alt :tmin :tmax :doy  :kRs :a :s])]
      (timing/microbenchmark
       {:consume-output #(if (class %) 1.0)}
       (solar-rad-as-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205})
       (solar-rad-pos-graph 45.0 100.0 15.0 25.0 205)
       (solar-rad-pos-graph-th 45.0 100.0 15.0 25.0 205)
       (solar-rad-from-temp-fn 45.0 100.0 25.0 15.0 205)))))

;; As graph
;; "Elapsed time: 572.225293 msecs"
;; "Elapsed time: 318.7705 msecs"
;; "Elapsed time: 279.98468 msecs"
;; "Elapsed time: 277.022995 msecs"
;; "Elapsed time: 278.224107 msecs"
;; "Elapsed time: 276.357795 msecs"
;; "Elapsed time: 274.711719 msecs"
;; "Elapsed time: 277.10278 msecs"
;; "Elapsed time: 275.60973 msecs"
;; "Elapsed time: 276.651091 msecs"
;; As fn
;; "Elapsed time: 22.0288 msecs"
;; "Elapsed time: 16.1642 msecs"
;; "Elapsed time: 9.863406 msecs"
;; "Elapsed time: 9.611822 msecs"
;; "Elapsed time: 10.153585 msecs"
;; "Elapsed time: 10.043576 msecs"
;; "Elapsed time: 9.623545 msecs"
;; "Elapsed time: 9.710815 msecs"
;; "Elapsed time: 17.697419 msecs"
;; "Elapsed time: 9.881647 msecs"
