(ns plumbing.graph-perf-test
  "Simple performance test based on example graph from Climate."
  (:use
   [plumbing.core :only [fnk]])
  (:require
   [plumbing.graph :as graph]))

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
  (-> (* day-of-year 2 Math/PI)
      (/ days-in-year)
      Math/cos
      (* 0.033)
      inc))

(defn solar-declination
  "Solar declination at a given day of the year (1 to 366)"
  ^double [^long day-of-year]
  (-> (* day-of-year Math/PI 2)
      (/ days-in-year)
      (- 1.39)
      Math/sin
      (* 0.409)))

(defn sunset-hour-angle
  "Sunset hour angle given day of year (1 to 366) and latitude in radians"
  ^double [^double solar-dec ^double lat-in-rad]
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
   :Rn  (fnk [Rns Rnl] (- Rns Rnl))})

(defn solar-rad-from-temp-fn
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
       Rn)))

(defmacro fn-call
  [args body]
  `((fn ~args ~body) ~@args))
(defrecord SolarRadRecord [Ra Rs Rso Rns tmax-kelvin tmin-kelvin ea term-1
                           term-2 term-3 Rnl Rn])
(defn solar-rad-from-temp-fn-calls
  ([lat alt tmax tmin day-of-year]
     (let [kRs 0.16
           a 0.23]
       (solar-rad-from-temp-fn-calls kRs lat alt tmax tmin day-of-year a)))
  ([kRs lat alt tmax tmin day-of-year]
     (let [a 0.23]
       (solar-rad-from-temp-fn-calls kRs lat alt tmax tmin day-of-year a)))
  ([kRs lat alt tmax tmin day-of-year a]
     (let [Ra (fn-call [day-of-year lat] (solar-rad-et day-of-year lat))
           Rs (fn-call [tmax tmin kRs Ra] (-> (- tmax tmin) Math/sqrt (* kRs Ra)))
           Rso (fn-call [alt Ra] (-> (* 2e-5 alt) (+ 0.75) (* Ra)))
           Rns (fn-call [a Rs] (-> (- 1 a) (* Rs)))
           tmaxK (fn-call [tmax] (to-kelvin tmax))
           tminK (fn-call [tmin] (to-kelvin tmin))
           ea (fn-call [tmin] (sat-vapour-pressure tmin))
           term1 (fn-call [tmaxK tminK]
                          (-> (Math/pow tmaxK 4) (+ (Math/pow tminK 4)) (* 4.903e-9) (/ 2)))
           term2 (fn-call [ea] (-> (Math/sqrt ea) (* -0.14) (+ 0.34)))
           term3 (fn-call [Rs Rso] (-> (* 1.35 Rs) (/ Rso) (- 0.35)))
           Rnl (fn-call [term1 term2 term3] (* term1 term2 term3))
           Rn  (fn-call [Rns Rnl] (- Rns Rnl))]
       (new SolarRadRecord Ra Rs Rso Rns tmaxK tminK ea term1 term2 term3 Rnl Rn))))

(defn -main
  [& args]
  ;; Simple profiling, comparing the various compilations to the let
  ;; implementation.

  (println "Interpreted eager")
  (println "  compiling")
  (let [solar-rad-as-graph (time (graph/interpreted-eager-compile solar-rad-from-temp))]
    (println "  gives value" (solar-rad-as-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205}))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-as-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205})))))

  (println)
  (println "Eager called with map")
  (println "  compiling")
  (let [solar-rad-pos-graph (time (graph/eager-compile solar-rad-from-temp))]
    (println "  gives value"  (solar-rad-pos-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205}))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-pos-graph {:lat 45.0 :alt 100.0 :tmin 15.0 :tmax 25.0 :doy 205})))))

  (println)
  (println "Eager positional fn")
  (println "  compiling")
  (let [solar-rad-pos-graph-pos (time (graph/positional-eager-compile
                                       (into {} solar-rad-from-temp)
                                       [:lat :alt :tmin :tmax :doy]))]
    (println "  gives value" (solar-rad-pos-graph-pos 45.0 100.0 15.0 25.0 205))
    (dotimes [_ 10]
      (time (dotimes [_ 10000]
              (solar-rad-pos-graph-pos 45.0 100.0 15.0 25.0 205)))))

  (println)
  (println "Let with fn calls")
  (println "  no need to compile")
  (println "  gives value" (solar-rad-from-temp-fn-calls 45.0 100.0 25.0 15.0 205))
  (dotimes [_ 10]
    (time (dotimes [_ 10000]
            (solar-rad-from-temp-fn-calls 45.0 100.0 25.0 15.0 205))))

  (println)
  (println "Let")
  (println "  no need to compile")
  (println "  gives value" (solar-rad-from-temp-fn 45.0 100.0 25.0 15.0 205))
  (dotimes [_ 10]
    (time (dotimes [_ 10000]
            (solar-rad-from-temp-fn 45.0 100.0 25.0 15.0 205)))))

;;Interpreted eager
;;  compiling
;;"Elapsed time: 6.037156 msecs"
;;  gives value {:Rns 15.392102866343723, :tminK 288.15, :Rso 29.71016678897226, :term2 0.15717553186329483, :Rn 12.209062049816033, :Rnl 3.1830408165276896, :term3 0.5583137960058112, :term1 36.27261861695186, :Rs 19.989743982264574, :tmaxK 298.15, :ea 1.7053462321157722, :Ra 39.508200517250344}
;;"Elapsed time: 391.094708 msecs"
;;"Elapsed time: 341.611376 msecs"
;;"Elapsed time: 333.087152 msecs"
;;"Elapsed time: 367.831138 msecs"
;;"Elapsed time: 341.161334 msecs"
;;"Elapsed time: 324.572381 msecs"
;;"Elapsed time: 337.84041 msecs"
;;"Elapsed time: 336.182569 msecs"
;;"Elapsed time: 360.237391 msecs"
;;"Elapsed time: 371.491237 msecs"
;;
;;Eager called with map
;;  compiling
;;"Elapsed time: 36.108162 msecs"
;;  gives value #user.graph-record1401{:Rns 15.392102866343723, :tminK 288.15, :Rso 29.71016678897226, :term2 0.15717553186329483, :Rn 12.209062049816033, :Rnl 3.1830408165276896, :term3 0.5583137960058112, :term1 36.27261861695186, :Rs 19.989743982264574, :tmaxK 298.15, :ea 1.7053462321157722, :Ra 39.508200517250344}
;;"Elapsed time: 29.617148 msecs"
;;"Elapsed time: 28.233836 msecs"
;;"Elapsed time: 29.150146 msecs"
;;"Elapsed time: 28.360114 msecs"
;;"Elapsed time: 28.416531 msecs"
;;"Elapsed time: 38.486866 msecs"
;;"Elapsed time: 25.48484 msecs"
;;"Elapsed time: 27.788339 msecs"
;;"Elapsed time: 30.93764 msecs"
;;"Elapsed time: 25.088613 msecs"
;;
;;Eager positional fn
;;  compiling
;;"Elapsed time: 40.813282 msecs"
;;  gives value #user.graph-record1500{:Rns 15.392102866343723, :tminK 288.15, :Rso 29.71016678897226, :term2 0.15717553186329483, :Rn 12.209062049816033, :Rnl 3.1830408165276896, :term3 0.5583137960058112, :term1 36.27261861695186, :Rs 19.989743982264574, :tmaxK 298.15, :ea 1.7053462321157722, :Ra 39.508200517250344}
;;"Elapsed time: 15.038361 msecs"
;;"Elapsed time: 13.721086 msecs"
;;"Elapsed time: 13.787 msecs"
;;"Elapsed time: 16.328639 msecs"
;;"Elapsed time: 14.597608 msecs"
;;"Elapsed time: 13.985261 msecs"
;;"Elapsed time: 13.96927 msecs"
;;"Elapsed time: 13.764979 msecs"
;;"Elapsed time: 14.824762 msecs"
;;"Elapsed time: 13.781415 msecs"
;;
;;Let with fn calls
;;  no need to compile
;;  gives value #plumbing.graph_perf_test.SolarRadRecord{:Ra 39.508200517250344, :Rs 19.989743982264574, :Rso 29.71016678897226, :Rns 15.392102866343723, :tmax-kelvin 298.15, :tmin-kelvin 288.15, :ea 1.7053462321157722, :term-1 36.27261861695186, :term-2 0.15717553186329483, :term-3 0.5583137960058112, :Rnl 3.1830408165276896, :Rn 12.209062049816033}
;;"Elapsed time: 15.278042 msecs"
;;"Elapsed time: 12.319934 msecs"
;;"Elapsed time: 12.543768 msecs"
;;"Elapsed time: 12.316507 msecs"
;;"Elapsed time: 12.812431 msecs"
;;"Elapsed time: 12.724281 msecs"
;;"Elapsed time: 13.105688 msecs"
;;"Elapsed time: 12.757586 msecs"
;;"Elapsed time: 12.865573 msecs"
;;"Elapsed time: 12.54549 msecs"
;;
;;Let
;;  no need to compile
;;  gives value 12.209062049816033
;;"Elapsed time: 11.762075 msecs"
;;"Elapsed time: 10.430211 msecs"
;;"Elapsed time: 10.343818 msecs"
;;"Elapsed time: 10.372467 msecs"
;;"Elapsed time: 10.906296 msecs"
;;"Elapsed time: 10.285573 msecs"
;;"Elapsed time: 10.464272 msecs"
;;"Elapsed time: 10.357106 msecs"
;;"Elapsed time: 10.409813 msecs"
;;"Elapsed time: 10.512621 msecs"
