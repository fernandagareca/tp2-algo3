(ns tp2.core
  (:require [clojure.string :as str]))

(defn crear-linea
  "pre:x1, y1, x2, y2 son números que representan coordenadas en un espacio 2D.
   post:Devuelve una cadena que representa una línea SVG"
  [x1 y1 x2 y2 ancho-trazo]
  (str "<line x1=\"" x1 "\" y1=\"" y1 "\" x2=\"" x2 "\" y2=\"" y2
       "\" stroke-width=\"" ancho-trazo "\" stroke=\"black\" />\n"))

(defn parsear-letra
  "pre: tortuga es un mapa que contiene las claves :pos, :angulo, :pluma-abajo, :cola, :min-x, :max-x, :min-y, :max-y.
        letra contiene los comandos válidos (F, G, f, g, +, -, |, [, ]).
     post: Devuelve un mapa actualizado que representa el nuevo estado de la tortuga"
  [tortuga letra angulo-mov ancho-trazo]
  (let [{:keys [pos angulo pluma-abajo cola min-x max-x min-y max-y] :as tortuga} tortuga]
    (cond
      (or (= letra \F) (= letra \G))
      (let [nuevo-x (+ (first pos) (* (Math/cos (Math/toRadians angulo)) 10))
            nuevo-y (+ (second pos) (* (Math/sin (Math/toRadians angulo)) 10))
            min-x (min min-x nuevo-x)
            max-x (max max-x nuevo-x)
            min-y (min min-y nuevo-y)
            max-y (max max-y nuevo-y)]
        (if pluma-abajo
          (assoc tortuga :pos [nuevo-x nuevo-y]
                 :min-x min-x :max-x max-x :min-y min-y :max-y max-y
                 :lineas (conj (tortuga :lineas) (crear-linea (first pos) (second pos) nuevo-x nuevo-y ancho-trazo)))
          (assoc tortuga :pos [nuevo-x nuevo-y]
                 :min-x min-x :max-x max-x :min-y min-y :max-y max-y)))
      (or (= letra \f) (= letra \g)) (assoc tortuga :pluma-abajo (not pluma-abajo))
      (= letra \+) (assoc tortuga :angulo (+ angulo angulo-mov))
      (= letra \-) (assoc tortuga :angulo (- angulo angulo-mov))
      (= letra \|) (assoc tortuga :angulo (+ angulo 180))
      (= letra \[) (assoc tortuga :cola (conj cola {:pos pos :angulo angulo :pluma-abajo pluma-abajo}))
      (= letra \]) (let [estado (peek cola) nueva-cola (pop cola)]
                     (merge tortuga estado {:cola nueva-cola}))
      :else
      tortuga)))

(defn parsear-comandos
  "pre:comandos es una secuencia de comandos de tortuga válidos.
        angulo-mov es el ángulo de rotación en grados.
   post:Parsea los comandos enviados por parámetro, devolviendo el contenido en líneas SVG."
  [comandos angulo-mov ancho-trazo]
  (let [tortuga-inicial {:pos [0 0] :angulo 270 :pluma-abajo true :lineas [] :cola []
                         :min-x 0 :max-x 0 :min-y 0 :max-y 0}]
    (reduce (fn [tortuga letra] (parsear-letra tortuga letra angulo-mov ancho-trazo)) tortuga-inicial comandos)))

(defn crear-archivo-svg!
  "pre:nombre-archivo es el archivo svg que debe crearse.
   post:crea un archivo SVG dado el nombre del archivo y los comandos de la tortuga ajusta para que la imagen entre"
  [nombre-archivo letras anguloMov]
  (let [resultado (parsear-comandos letras anguloMov 1)
        min-x (:min-x resultado)
        max-x (:max-x resultado)
        min-y (:min-y resultado)
        max-y (:max-y resultado)
        ancho (- max-x min-x)  
        alto (- max-y min-y) 
        ancho-trazo (Math/ceil (/ ancho 5000)) 
        resultado (parsear-comandos letras anguloMov ancho-trazo) 
        lineas (apply str (get resultado :lineas))
        final (str "<svg viewBox=\""(str min-x " " min-y " " ancho " " alto)"\" xmlns=\"http://www.w3.org/2000/svg\">\n" lineas "\n</svg>")] 
    (spit nombre-archivo final)
    )
  )

(defn parse-regla [line]
  (let [[pre post] (str/split line #" " 2)]
    [(keyword (str/trim pre)) (str/trim post)]))

(defn parsear-cadena
  "pre: cadena-reglas son las instrucciones de la tortuga. 
   post:Devuelve un vector de caracteres que componen la cadena cadena-reglas"
  [cadena-reglas]
  (->> cadena-reglas seq vec))

(defn leer-archivo!
  "pre:es una cadena que representa el nombre de un archivo existente que contiene el ángulo, axioma y reglas.
   post:Devuelve un mapa con las claves :angulo (un número), :axioma (una cadena de texto), y :reglas (un mapa de reglas de transformación).
   Si el archivo no existe, devuelve nil."
  [nombre-archivo]
   (try
     (let [contenido (slurp nombre-archivo)
           lineas (str/split-lines contenido)
           angulo (Float/parseFloat (nth lineas 0))
           axioma (nth lineas 1)
           reglas (map parse-regla (drop 2 lineas))
           reglas-map (apply hash-map (flatten reglas))]
       {:angulo angulo :axioma axioma :reglas reglas-map})
     (catch java.io.FileNotFoundException _ nil)))

(defn devolver-cadenas
  "pre:valor es una cadena que representa la secuencia inicial,reglas es un mapa de reglas de transformación.
      cantidad-iteraciones es un número entero no negativo.
   post:Devuelve una cadena de texto que resulta de aplicar las reglas de transformación a valor durante cantidad-iteraciones."
  [valor reglas cantidad-iteraciones]
  (if (> cantidad-iteraciones 0)
    (let [nuevo-valor (apply str (map #(get reglas (keyword (str %)) (str %)) valor))]
      (devolver-cadenas nuevo-valor reglas (dec cantidad-iteraciones)))
    valor))

(defn archivo-comandos
  "pre:nombre es una cadena que representa el nombre de un archivo existente que contiene el ángulo, axioma y reglas.
        iteraciones es un número entero no negativo.
   post:Devuelve una secuencia de caracteres con los comandos (F + - | f...)"
  [nombre iteraciones]
  (let [datos (leer-archivo! nombre)
        cadena (devolver-cadenas (str (get datos :axioma)) (get datos :reglas) iteraciones)]
    (parsear-cadena cadena)))

(defn -main [& args]
  (if (= (count args) 3)
    (let [nombre-archivo (first args)
          cant-iteraciones (Integer/parseInt (second args))
          nombre-salida (nth args 2)
          hash-datos (leer-archivo! nombre-archivo)
          comandos (archivo-comandos nombre-archivo cant-iteraciones)]
      (crear-archivo-svg! nombre-salida comandos (get hash-datos :angulo)))
    nil))