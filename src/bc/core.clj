(ns bc.core
  (:refer-clojure :exclude [==])
  (:require [clojure.java.io :as io]
            [clojure.core.logic :refer :all])
  (:import (org.objectweb.asm  ClassReader Type)
           (org.objectweb.asm.tree ClassNode)))

(def var-class
  "/Users/hiredman/src/clojure/target/classes/clojure/lang/Var.class")

(defn f []
  (let [cn (ClassNode.)]
    (.accept (ClassReader. (io/input-stream var-class))
             cn
             1)
    cn))

(extend-type org.objectweb.asm.tree.MethodNode
  IUnifyWithMap
  (unify-with-map [v u s]
    (loop [s s k (keys u)]
      (if (seq k)
        (if-let [value (case (first k)
                         :name (.name v)
                         :signature (.signature v)
                         :desc (.desc v))]
          (if-let [s (unify s (get u (first k)) value)]
            (recur s (next k))
            false)
          false)
        s)))
  IUnifyTerms
  (unify-terms [u v s]
    (if (map? v)
      (unify-with-map u v s)
      (unify-terms v u s))))

(defn fieldo [fields result]
  (fresh [a b c d e f]
         (conde
          ((== fields ()) (== result []))
          ((firsto fields a)
           (resto fields b)
           (fieldo b c)
           (firsto a d)
           (resto a e)
           (firsto e f)
           (conso [d " = " f "\n"] c result)))))

(defn methodo [methods result]
  (fresh [m ms r1 name s]
         (conde
          ((== methods ()) (== result []))
          (;; (firsto methods m)
           ;; (resto methods ms)
           (conso m ms methods)
           (== m {:name name
                  :desc s})
           (methodo ms r1)
           (conso [name " " s "\n"] r1 result)))))

(defn classo [class-node result]
  (fresh [class-preamble
          class-footer
          fields
          methods]
         (== result [class-preamble
                     fields
                     methods
                     class-footer])
         (== class-preamble ["class "
                             (.replace (.name class-node) "/" "_")
                             " << "
                             (.replace (.superName class-node) "/" "_")
                             "\n"])
         (== class-footer ["\nend\n"])
         (methodo (seq (.methods class-node)) methods)
         (fieldo (map (juxt #(.name %)
                            #(.value %))
                      (.fields class-node)) fields)))

(defn h [cn]
  (run 2 [q]
       (classo cn q)))

(defn out [x]
  (if (coll? x)
    (doseq [x (seq x)]
      (out x))
    (print x)))

(defprotocol Code
  (make-ruby [code opts]))

(defn ruby-method-name [f]
  (cond
   (= f "toString") "to_s"
   (= f "<init>") "initialize"
   (= f "<clinit>")  "static_block"
   :else (map (fn [x]
                (if (Character/isUpperCase x)
                  (str "_" (Character/toLowerCase x))
                  x))
              f)))

(defn ruby-args [desc]
  [\(
   (interpose ", "
              (map-indexed
               (fn [idx t]
                 (str "arg_" (inc idx)))
               (.getArgumentTypes (Type/getMethodType desc))))
   \)])

(extend-protocol Code
  org.objectweb.asm.tree.ClassNode
  (make-ruby [class-node opts]
    ["class "
     (.replace (.name class-node) "/" "_")
     " << "
     (.replace (.superName class-node) "/" "_")
     "\n"
     "attr_accessors "
     (butlast
      (for [f (.fields class-node)
            x [" :" (.name f) ","]]
        x))
     "\n"
     (for [m (sort-by #(.name %) (.methods class-node))]
       (do
         (.accept m class-node)
        ["  def " (ruby-method-name (.name m)) " " (ruby-args (.desc m)) "\n"
         (.size (.instructions m))
         "\n"
         "  end" "\n"]))
     "\nend\n"]))

(defn class-resolve [idx class]
  (when (> (count (:constant-pool class)) idx -1)
    (let [o (nth (:constant-pool class) idx)]
      (if (vector? o)
        (case (nth o 0)
          (10 11) {:class (class-resolve (nth o 1) class)
                   :method (class-resolve (nth o 2) class)}
          9 {:class (class-resolve (nth o 1) class)
             :field (class-resolve (nth o 2) class)}
          7 (.replace (class-resolve (nth o 1) class) "/" ".")
          12 {:name (class-resolve (nth o 1) class)
              :descriptor (class-resolve (nth o 2) class)}
          8 (class-resolve (nth o 1) class))
        o))))

(defn read-code [bytes class]
  (let [code (atom {})]
    (with-open [b (java.io.DataInputStream. (java.io.ByteArrayInputStream. bytes))]
      (swap! code assoc
             :max-stack (.readShort b)
             :max-locals (.readShort b)
             :code-length (.readInt b))
      (let [bc (byte-array (:code-length @code))]
        (.read b bc)
        (swap! code assoc :code
               (let [bv (vec (map #(bit-and (int %) 0xff) bc))]
                 (vec
                  ((fn f [x]
                     (lazy-seq
                      (when (> (count bv) x)
                        (case (nth bv x)
                          182 (cons [x :invokevirtual
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          183 (cons [x :invokespecial
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          185 (cons [x :invokeinterface
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 5 x)))
                          184 (cons [x :invokestatic
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          180 (cons [x :getfield
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          178 (cons [x :getstatic
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          167 (cons [x :goto
                                     (+
                                      (bit-shift-left (nth bv (inc x)) 8)
                                     (nth bv (+ x 2)))]
                                    (f (+ 3 x)))
                          187 (cons [x :new
                                     (class-resolve
                                      (dec (+
                                            (bit-shift-left (nth bv (inc x)) 8)
                                            (nth bv (+ x 2))))
                                      class)]
                                    (f (+ 3 x)))
                          198 (cons [x :ifnull
                                     (+
                                      (bit-shift-left (nth bv (inc x)) 8)
                                      (nth bv (+ x 2)))]
                                    (f (+ 3 x)))
                          18 (cons [x :ldc (class-resolve
                                          (dec (nth bv (inc x)))
                                          class)]
                                   (f (+ 2 x)))
                          58 (cons [x :astore (nth bv (inc x))]
                                   (f (+ 2 x)))
                          25 (cons [x :aload (nth bv (inc x))]
                                   (f (+ 2 x)))
                          29 (cons [x :iload_3]
                                   (f (inc x)))
                          42 (cons [x :aload_0]
                                   (f (inc x)))
                          43 (cons [x :aload_1]
                                   (f (inc x)))
                          44 (cons [x :aload_2]
                                   (f (inc x)))
                          176 (cons [x :areturn]
                                    (f (inc x)))
                          4 (cons [x :iconst_1]
                                  (f (inc x)))
                          88 (cons [x :pop2]
                                   (f (inc x)))
                          89 (cons [x :dup]
                                   (f (inc x)))
                          192 (cons [x :athrow]
                                    (f (inc x)))
                          77 (cons [x :astore_2]
                                   (f (inc x)))
                          78 (cons [x :astore_3]
                                   (f (inc x)))
                          45 (cons [x :aload_3]
                                   (f (inc x)))
                          153 (cons [x :ifeq
                                     (+
                                      (bit-shift-left (nth bv (inc x)) 8)
                                      (nth bv (+ x 2)))]
                                    (f (+ 3 x)))
                          154 (cons [x :ifne
                                     (+
                                      (bit-shift-left (nth bv (inc x)) 8)
                                      (nth bv (+ x 2)))]
                                    (f (+ 3 x)))
                          (cons [x (nth bv x)]
                                (f (inc x))))))) 0)))))
      ;; TODO: exceptions, attributes
      )
    @code))

(defn modifiers [arg]
  (set
   (for [[k v] {:native (. java.lang.reflect.Modifier isNative arg),
                :synchronized (. java.lang.reflect.Modifier isSynchronized arg),
                :protected (. java.lang.reflect.Modifier isProtected arg),
                :transient (. java.lang.reflect.Modifier isTransient arg),
                :private (. java.lang.reflect.Modifier isPrivate arg),
                :static (. java.lang.reflect.Modifier isStatic arg),
                :public (. java.lang.reflect.Modifier isPublic arg),
                :volatile (. java.lang.reflect.Modifier isVolatile arg),
                :interface (. java.lang.reflect.Modifier isInterface arg),
                :final (. java.lang.reflect.Modifier isFinal arg),
                :strict (. java.lang.reflect.Modifier isStrict arg),
                :abstract (. java.lang.reflect.Modifier isAbstract arg)}
         :when v]
     k)))

(defn parse-descriptor [desc]
  (let [t (Type/getMethodType desc)]
    [(vec (map #(symbol (.getClassName %))
               (.getArgumentTypes t)))
     (symbol (.getClassName (.getReturnType t)))]))

(defn read-class [x]
  (let [m (atom {})]
    (with-open [r (java.io.DataInputStream. (io/input-stream x))]
      (let [x (.readInt r)]
        (assert (= (Integer/toHexString x)
                   (Long/toHexString 0xCAFEBABE))))
      (let [minor-version (.readShort r)
            major-version (.readShort r)]
        (swap! m assoc
               :minor-version minor-version
               :major-version major-version)
        (assert (>= major-version 49) major-version))
      (swap! m assoc
             :constant-pool-size (- (.readShort r) 1)
             :constant-pool [])
      (dotimes [i (:constant-pool-size @m)]
        (let [tag (.readByte r)]
          (case tag
            1 (swap! m update-in [:constant-pool] conj (.readUTF r))
            (8 7) (swap! m update-in [:constant-pool] conj
                         (conj (vector-of :short)
                               tag
                               (dec (.readShort r))))
            (9 10 11 12) (swap! m update-in [:constant-pool] conj
                                (conj (vector-of :short)
                                      tag
                                      (dec (.readShort r))
                                      (dec (.readShort r))))
            )))
      (swap! m assoc
             :access-flags (.readShort r)
             :self (class-resolve (dec (.readShort r)) @m)
             :super (class-resolve (dec (.readShort r)) @m)
             :interface-count (.readShort r)
             :interfaces #{})
      (dotimes [i (:interface-count @m)]
        (swap! m update-in [:interfaces]
               conj (class-resolve (dec (.readShort r)) @m)))
      (swap! m assoc
             :field-count (.readShort r)
             :fields [])
      (dotimes [i (:field-count @m)]
        (let [field (atom {:access (.readShort r)
                           :name (class-resolve (dec (.readShort r)) @m)
                           :descriptor (dec (.readShort r))
                           :attribute-count (.readShort r)
                           :attributes []})]
          (dotimes [ii (:attribute-count @field)]
            (let [name (dec (.readShort r))
                  length (.readInt r)
                  info (byte-array length)]
              (dotimes [iii length]
                (aset info iii (.readByte r)))
              (swap! field update-in [:attributes] conj
                     {:name name
                      :info info})))
          (swap! m update-in [:fields] conj @field)))
      (swap! m assoc
             :methods-count (.readShort r)
             :methods #{})
      (dotimes [i (:methods-count @m)]
        (let [method (atom {:access (modifiers (.readShort r))
                            :name (symbol (class-resolve (dec (.readShort r)) @m))
                            :descriptor (class-resolve (dec (.readShort r)) @m)
                            :attributes-count (.readShort r)
                            :attributes []})
              [args return] (parse-descriptor (:descriptor @method))]
          (swap! method assoc
                 :return return
                 :args args)
          (dotimes [ii (:attributes-count @method)]
            (let [name (class-resolve (dec (.readShort r)) @m)
                  length (.readInt r)
                  info (byte-array length)]
              (dotimes [iii length]
                (aset info iii (.readByte r)))
              (swap! method update-in [:attributes] conj
                     {:name name
                      :info (if (= name "Code")
                              (read-code info @m)
                              info)})))
          (swap! m update-in [:methods] conj @method)))
      (swap! m assoc
             :attributes-count (.readShort r)
             :attributes [])
      (dotimes [i (:attributes-count @m)]
        (let [name (class-resolve (dec (.readShort r)) @m)
                  length (.readInt r)
                  info (byte-array length)]
              (dotimes [iii length]
                (aset info iii (.readByte r)))
              (swap! m update-in [:attributes] conj
                     {:name name
                      :info info}))))
    (dissoc @m :constant-pool)))
