;;https://gist.github.com/707630
;;http://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
;;http://docs.oracle.com/javase/specs/jvms/se5.0/html/ClassFile.doc.html
(ns bc.core
  (:refer-clojure :exclude [==])
  (:require [clojure.java.io :as io]
            [clojure.core.logic :refer :all]
            [bc.modifiers :refer [modifiers]])
  (:import (org.objectweb.asm Type)))

(set! *unchecked-math* true)

(def var-class
  "/Users/hiredman/src/clojure/target/classes/clojure/lang/Var.class"
  #_"/Users/hiredman/src/clojure/target/classes/clojure/lang/Box.class"
  #_"/Users/hiredman/src/clojure/target/classes/clojure/lang/Symbol.class")

(defn out [x]
  (if (coll? x)
    (doseq [x (seq x)]
      (out x))
    (print x)))

;; (defn parse-descriptor [desc]
;;   (letfn [(parse-method-descriptor [desc]
;;             (loop [desc desc
;;                    result []]
;;               (if-not (= \) (.charAt desc 0))
;;                 (let [a-desc (parse-descriptor desc)]
;;                   (recur (subs desc (count a-desc) (count desc))
;;                          (conj result a-desc)))
;;                 [result (parse-descriptor (subs desc 1))])))]
;;     (case (.charAt desc 0)
;;       \( (parse-method-descriptor (subs desc 1))
;;       \L (subs desc 0 (inc (.indexOf desc ";")))
;;       (str (.charAt desc 0)))))

(defn class-resolve [idx class]
  (when (> (count (:constant-pool class)) idx -1)
    (let [o (nth (:constant-pool class) idx)]
      (if (vector? o)
        (case (nth o 0)
          (10 11) {:class (class-resolve (nth o 1) class)
                   :method (class-resolve (nth o 2) class)}
          9       {:class (class-resolve (nth o 1) class)
                   :field (class-resolve (nth o 2) class)}
          7       (.replace (class-resolve (nth o 1) class) "/" ".")
          12      {:name (class-resolve (nth o 1) class)
                   :descriptor (class-resolve (nth o 2) class)}
          8       (class-resolve (nth o 1) class))
        o))))

(derive ::invokevirtual ::invoke)
(derive ::invokespecial ::invoke)
(derive ::invokeinterface ::invoke)
(derive ::invokestatic ::invoke)

(derive ::goto ::jump)
(derive ::ifnull ::jump)
(derive ::ifeq ::jump)
(derive ::ifne ::jump)
(derive ::ifnonnull ::jump)
(derive ::ifacmpeq ::jump)

(derive ::getfield ::field)
(derive ::putfield ::field)
(derive ::getstatic ::field)
(derive ::putstatic ::field)


(defmulti process-instruction (fn [[_ i & _] & _] i))

(defmethod process-instruction :default [[n i args] & _]
  (vec (list* n i args)))

(defmethod process-instruction ::invoke [[n i args] class]
  [n (keyword (name i))
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction ::field [[n i args] class]
  [n (keyword (name i))
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction :new [[n i args] class]
  [n i
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction :instanceof [[n i args] class]
  [n i
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction :checkcast [[n i args] class]
  [n i
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction :anewarray [[n i args] class]
  [n i
   (class-resolve (dec (+
                        (bit-shift-left (nth args 0) 8)
                        (nth args 1)))
                  class)])

(defmethod process-instruction :ldc [[n i args] class]
  [n i (class-resolve (dec (nth args 0)) class)])

(defmethod process-instruction ::jump [[n i args] class]
  [n (keyword (name i)) (short (+ n (bit-shift-left (nth args 0) 8) (nth args 1)))])

(def byte-code-map
  {;;invokes
   182 [::invokevirtual 2]
   183 [::invokespecial 2]
   185 [::invokeinterface 4]
   184 [::invokestatic 2]
   ;;jumps
   167 [::goto 2]
   198 [::ifnull 2]
   153 [::ifeq 2]
   154 [::ifne 2]
   199 [::ifnonnull 2]
   166 [::ifacmpeq 2]
   ;;stores
   58  [:astore 1]
   76  [:astore_1 0]
   77  [:astore_2 0]
   78  [:astore_3 0]
   75  [:astore_0 0]
   62  [:istore_3 0]
   ;;loads
   25  [:aload 1]
   29  [:iload_3 0]
   42  [:aload_0 0]
   43  [:aload_1 0]
   44  [:aload_2 0]
   45  [:aload_3 0]
   33  [:lload_3 0]
   27  [:iload_1 0]
   ;;stack twiddling
   88  [:pop2 0]
   89  [:dup 0]
   87  [:pop 0]
   90  [:dup_x1 0]
   ;;fields
   180 [::getfield 2]
   181 [::putfield 2]
   178 [::getstatic 2]
   179 [::putstatic 2]
   ;;return
   176 [:areturn 0]
   177 [:return 0]
   172 [:ireturn 0]
   ;;type checks
   192 [:checkcast 2]
   193 [:instanceof 2]
   ;;math
   96  [:iadd 0]
   ;;constants
   1   [:aconst_null 0]
   18  [:ldc 1]
   3   [:iconst_0 0]
   4   [:iconst_1 0]
   5   [:iconst_2 0]
   2   [:iconst_m1 0]
   ;;misc
   187 [:new 2]
   191 [:athrow 0]
   189 [:anewarray 2]
   83  [:aastore 0]})

(defmulti read-attribute (fn [name bytes class] name))

(defmethod read-attribute :default [name bytes class] bytes)

(defmethod read-attribute "SourceFile" [name bytes class]
  (class-resolve (dec (+ (bit-shift-left (nth bytes 0) 8) (nth bytes 1)))
                 class))

(defmethod read-attribute "Code" [name bytes class]
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
                        (if (contains? byte-code-map (nth bv x))
                          (let [[instruction argc] (get byte-code-map (nth bv x))
                                args (map (partial nth bv)
                                          (range (inc x)
                                                 (+ x (inc argc))))]
                            (cons (process-instruction [x instruction args] class)
                                  (f (+ (inc argc) x))))
                          (cons [x (nth bv x)]
                                (f (inc x))))))) 0)))))
      (swap! code assoc
             :exception-length (.readShort b)
             :exceptions [])
      (dotimes [i (:exception-length @code)]
        (swap! code update-in [:exceptions] conj [(.readShort b)
                                                  (.readShort b)
                                                  (.readShort b)
                                                  (let [idx (.readShort b)]
                                                    (if (zero? idx)
                                                      :finally
                                                      (class-resolve (dec idx) class)))]))
      (swap! code assoc
             :attributes-count (.readShort b)
             :attributes [])
      (dotimes [i (:attributes-count @code)]
        (let [name (class-resolve (dec (.readShort b)) class)
              length (.readInt b)
              info (byte-array length)]
          (.read b info)
          (swap! code update-in [:attributes] conj {:name name
                                                    :info (read-attribute name info class)}))))
    @code))

(defmethod read-attribute "LineNumberTable" [name info class]
  (let [linenumbers (atom {})]
    (with-open [b (java.io.DataInputStream. (java.io.ByteArrayInputStream. info))]
      (let [length (.readShort b)]
        (dotimes [i length]
          (swap! linenumbers assoc (.readShort b) (.readShort b)))))
    @linenumbers))

(defmethod read-attribute "LocalVariableTable" [name info class]
  (let [locals (atom [])]
    (with-open [b (java.io.DataInputStream. (java.io.ByteArrayInputStream. info))]
      (let [length (.readShort b)]
        (dotimes [i length]
          (swap! locals conj {:start (.readShort b)
                              :length (.readShort b)
                              :name (class-resolve (dec (.readShort b)) class)
                              :descriptor (class-resolve (dec (.readShort b)) class)
                              :index (.readShort b)}))))
    @locals))


(defn parse-descriptor [desc]
  (let [t (Type/getMethodType desc)]
    [(vec (map #(symbol (.getClassName %))
               (.getArgumentTypes t)))
     (symbol (.getClassName (.getReturnType t)))]))

(defn read-class [x]
  (let [m (atom {})]
    (with-open [r (java.io.DataInputStream. (io/input-stream x))]
      (let [x (.readInt r)]
        ;; this is embarrassing, but I couldn't get the comparison to
        ;; come out correctly unless I did it this way
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
             :access-flags (modifiers (.readShort r))
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
        (let [field (atom {:access (modifiers (.readShort r))
                           :name (class-resolve (dec (.readShort r)) @m)
                           :descriptor (class-resolve (dec (.readShort r)) @m)
                           :attribute-count (.readShort r)
                           :attributes []})]
          (dotimes [ii (:attribute-count @field)]
            (let [name (class-resolve (dec (.readShort r)) @m)
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
                      :info (read-attribute name info @m)})))
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
                  :info (read-attribute name info @m)}))))
    (dissoc @m
            :constant-pool
            :constant-pool-size)))

(defn ruby-method-name [method-name]
  (cond
   (= 'toString method-name) 'to_s
   (= '<clinit> method-name) 'self.static_init
   :else (munge (name method-name))))

(defn ruby-class-name [class-name]
  (.replace (last (.split (name class-name) "\\.")) "$" "_"))

(def bc->rb nil)

(defmulti bc->rb (fn [[ln op & args] stack code ptr method] op))

(defmethod bc->rb :aload_0 [[_ _ _] stack code ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (if ((:access method) :static)
                         (:name (get locals 0))
                         "self"))
     (inc ptr)]))

(defmethod bc->rb :putfield [[_ _ & args] stack code ptr _]
  [(let [value (peek-value stack)
         stack (pop-value stack)
         obj (peek-value stack)
         stack (pop-value stack)
         [{{:keys [name descriptor]} :field}] args
         [stack value] (if (= "Z" descriptor)
                         (let [b (gensym 'b)]
                           [(push-code (push-code stack
                                                  (format "%s = %s" b value))
                                       (format "%s = (%s == 0 or %s == false) ? false : true" b b b))
                            b])
                         [stack value])]
     (if (= obj "self")
       (push-code stack (format "@%s = %s" name value))
       (push-code stack (format "%s.%s = %s" obj name value))))
   (inc ptr)])

(defmethod bc->rb :iconst_1 [[_ _ _] stack _ ptr _]
  [(push-value stack "1") (inc ptr)])

(defmethod bc->rb :areturn [[_ _ _] stack _ ptr _]
  (let [value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (str "__return = " value)) (inc ptr)]))

(defmethod bc->rb :return [[_ _ _] stack _ ptr _]
  [stack (inc ptr)])

(defmethod bc->rb :ireturn [[_ _ _] stack _ ptr _]
  (let [value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (str "__return = " value)) (inc ptr)]))

(defmethod bc->rb :checkcast [[_ _ class] stack _ ptr _]
  [(push-code stack (str "#checkcast " class)) (inc ptr)])

(defmethod bc->rb :pop [[_ _ _] stack _ ptr _]
  [(pop-value stack) (inc ptr)])

(defmethod bc->rb :aload_1 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals 1))) (inc ptr)]))

(defmethod bc->rb :iload_1 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals 1))) (inc ptr)]))


(defmethod bc->rb :iload_3 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals 3))) (inc ptr)]))

(defmethod bc->rb :aload_2 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals 2))) (inc ptr)]))

(defmethod bc->rb :aload_3 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals 2))) (inc ptr)]))

(defmethod bc->rb :aload [[_ _ local] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))]
    [(push-value stack (:name (get locals local))) (inc ptr)]))

(defmethod bc->rb :ldc [[_ _ constant] stack _ ptr method]
  [(push-value stack (pr-str constant)) (inc ptr)])


(defmethod bc->rb :astore [[_ _ local] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))
        value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (format "%s = %s" (:name (get locals local)) value)) (inc ptr)]))

(defmethod bc->rb :aastore [[_ _ local] stack _ ptr method]
  (let [value (peek-value stack)
        stack (pop-value stack)
        index (peek-value stack)
        stack (pop-value stack)
        aref1 (peek-value stack)
        stack (pop-value stack)]
    [(push-value stack (str (subs aref1 0 (dec (count aref1)))
                            (when (> (count aref1) 2) ",")
                            value "]"))
     (inc ptr)]))

(defmethod bc->rb :astore_2 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))
        value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (format "%s = %s" (:name (get locals 2)) value)) (inc ptr)]))

(defmethod bc->rb :astore_3 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))
        value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (format "%s = %s" (:name (get locals 3)) value)) (inc ptr)]))

(defmethod bc->rb :astore_0 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))
        value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (format "%s = %s" (:name (get locals 0)) value)) (inc ptr)]))

(defmethod bc->rb :astore_1 [[_ _ _] stack _ ptr method]
  (let [locals (into {} (for [attr (:attributes method)
                              :when (= "Code" (:name attr))
                              attr (:attributes (:info attr))
                              :when (= "LocalVariableTable" (:name attr))
                              local (:info attr)]
                          [(:index local) local]))
        value (peek-value stack)
        stack (pop-value stack)]
    [(push-code stack (format "%s = %s" (:name (get locals 1)) value)) (inc ptr)]))

;; (defmethod bc->rb :istore_3 [[_ _ _] stack _ ptr method]
;;   (let [locals (into {} (for [attr (:attributes method)
;;                               :when (= "Code" (:name attr))
;;                               attr (:attributes (:info attr))
;;                               :when (= "LocalVariableTable" (:name attr))
;;                               local (:info attr)]
;;                           [(:index local) local]))
;;         value (peek stack)
;;         stack (pop stack)]
;;     [(conj stack (format "%s = %s" (:name (get locals 3)) value)) (inc ptr)]))

(defmethod bc->rb :invokestatic [[_ _ & args] stack _ ptr method]
  [(let [[{{:keys [name descriptor]} :method
           :keys [class]}] args
           [args return] (parse-descriptor descriptor)
           argc (count args)
           stacks (iterate pop-value stack)
           args (reverse (take argc (map peek-value stacks)))
           result (gensym 'invokestatic)]
     (push-value (push-code (nth stacks argc)
                            (format "%s = %s.%s(%s)"
                                    result
                                    (ruby-class-name class)
                                    name
                                    (apply str (interpose ", " args))))
                 result))
   (inc ptr)])

(defn method-call [[_ op & args] stack _ ptr method]
  [(let [[{{:keys [name descriptor]} :method
           :keys [class]}] args
           [args return] (parse-descriptor descriptor)
           argc (inc (count args))
           stacks (iterate pop-value stack)
           args (reverse (take argc (map peek-value stacks)))
           result (gensym (clojure.core/name op))]
     (push-value (push-code (nth stacks argc)
                            (format "%s = %s.%s(%s)"
                                    result
                                    (first args)
                                    name
                                    (apply str (interpose ", " (rest args)))))
                 result))
   (inc ptr)])

(defmethod bc->rb :invokevirtual [thing stack code ptr method]
  (method-call thing stack code ptr method))

(defmethod bc->rb :invokeinterface [thing stack code ptr method]
  (method-call thing stack code ptr method))

(defmethod bc->rb :getfield [[_ _ & args] stack _ ptr _]
  [(let [[{{:keys [name descriptor]} :field
           :keys [class]}] args
           obj (peek-value stack)
           stack (pop-value stack)]
     (push-value stack
                 (if (= obj "self")
                   (str "@" name)
                   (format "%s.%s" obj name))))
   (inc ptr)])

(defmethod bc->rb :new [[_ _ & args] stack _ ptr _]
  [(push-value stack (gensym 'new)) (inc ptr)])

(defmethod bc->rb :anewarray [[_ _ & args] stack _ ptr _]
  [(push-value stack "[]") (inc ptr)])

(defmethod bc->rb :dup [[_ _ _] stack _ ptr _]
  [(push-value stack (peek-value stack)) (inc ptr)])

(defmethod bc->rb :aconst_null [[_ _ & args] stack _ ptr _]
  [(push-value stack "nil") (inc ptr)])

(defmethod bc->rb :iconst_0 [[_ _ & args] stack _ ptr _]
  [(push-value stack "0") (inc ptr)])

(defmethod bc->rb :iconst_2 [[_ _ & args] stack _ ptr _]
  [(push-value stack "2") (inc ptr)])

;; (defmethod bc->rb :iconst_m1 [[_ _ & args] stack _ ptr _]
;;   [(conj stack "-1") (inc ptr)])

(defmethod bc->rb :iadd [[_ _ & args] stack _ ptr _]
  (let [n (peek-value stack)
        stack (pop-value stack)
        n+1 (peek-value stack)
        stack (pop-value stack)]
    [(push-value stack (str n " + " n+1)) (inc ptr)]))

(defmethod bc->rb :invokespecial [[n _ {:keys [class] {:keys [name descriptor]} :method}]
                                  stack code ptr method]
  (let [class (ruby-class-name class)
        [args return] (parse-descriptor descriptor)
        argc (inc (count args))
        stacks (iterate pop-value stack)
        [name & args] (reverse (take argc (map peek-value stacks)))
        stack (nth stacks argc)]
    (if (and (.startsWith (clojure.core/name (:name method)) "<init>")
             (= name "self"))
      [(push-code
        (push-code stack (str "#invoking own constructor "))
        (format "self._LT_init_GT_(%s)" (apply str (interpose \, args))))
       (inc ptr)]
      [(push-code
        (push-code stack "#invokespecial")
        (format "%s = %s.new(%s)" name class (apply str (interpose \, args))))
       (inc ptr)])))

(defmethod bc->rb :getstatic [[_ _ & args] stack _ ptr _]
  [(let [[{{:keys [name descriptor]} :field
           :keys [class]}] args]
     (push-value stack
                 (format "%s.%s" (ruby-class-name class) name)))
   (inc ptr)])

(defmethod bc->rb :putstatic [[_ _ & args] stack _ ptr _]
  [(let [[{{:keys [name descriptor]} :field
           :keys [class]}] args
           value (peek-value stack)
           stack (pop-value stack)]
     (push-code stack
                (format "%s.%s = %s" (ruby-class-name class) name value)))
   (inc ptr)])

(defmethod bc->rb :athrow [[_ _] stack _ ptr _]
  [(let [value (peek-value stack)
         stack (pop-value stack)]
     (push-code stack (format "raise %s " value)))
   (inc ptr)])

(defmethod bc->rb :instanceof [[_ _ & args] stack _ ptr _]
  [(let [[a-class] args
         obj (peek-value stack)
         stack (pop-value stack)]
     (push-value stack
                 (str obj ".instance_of?(" (ruby-class-name a-class) ")")))
   (inc ptr)])


(defmethod bc->rb :ifne [[_ _ label] stack code ptr method]
  (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
        value (peek-value stack)
        stack (pop-value stack)
        then (loop [ptr else
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))
        else (loop [ptr (inc ptr)
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))]
    [(reduce push-code else (concat [(str "(if " value " then")]
                                    (map (partial str "  ")
                                         (codes then))
                                    ["else"]
                                    (map (partial str "  ")
                                         (codes else))
                                    ["end)"]))
     (count code)]))



(defmethod bc->rb :ifeq [[_ _ label] stack code ptr method]
  (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
        value (peek-value stack)
        stack (pop-value stack)
        then (loop [ptr (inc ptr)
                    stack (drop-code stack)]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))
        else (loop [ptr else
                    stack (drop-code stack)]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))]
    [(reduce push-code (push-code (drop-code else) "#ifeq")
             (concat (codes stack)
                     [(str "(if " value " then")]
                     (map (partial str "  ")
                          (codes then))
                     ["else"]
                     (map (partial str "  ")
                          (codes else))
                     ["end)"]))
     (count code)]))

(defmethod bc->rb :ifnonnull [[_ _ label] stack code ptr method]
  (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
        value (peek-value stack)
        stack (pop-value stack)
        then (loop [ptr else
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))
        else (loop [ptr (inc ptr)
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))]
    [(reduce push-code else (concat [(str "(if (" value ").nil? then")]
                                    (map (partial str "  ")
                                         (codes then))
                                    ["else"]
                                    (map (partial str "  ")
                                         (codes else))
                                    ["end)"]))
     (count code)]))

(defmethod bc->rb :ifnull [[_ _ label] stack code ptr method]
  (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
        value (peek-value stack)
        stack (pop-value stack)
        then (loop [ptr (inc ptr)
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))
        else (loop [ptr else
                    stack stack]
               (if (> (count code) ptr)
                 (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
                   (recur ptr stack))
                 stack))]
    [(reduce push-code else (concat [(str "(if (" value ").nil? then")]
                                    (map (partial str "  ")
                                         (codes then))
                                    ["else"]
                                    (map (partial str "  ")
                                         (codes else))
                                    ["end)"]))
     (count code)]))


;; (defmethod bc->rb :ifnull [[_ _ label] stack code ptr method]
;;   (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
;;         value (peek stack)
;;         stack (pop stack)]
;;     [(conj stack
;;            (format "(if nil?(%s) then %s else %s end)"
;;                    value
;;                    (try
;;                      (apply str (interpose \newline
;;                                            (loop [ptr (inc ptr)
;;                                                   stack []]
;;                                              (if (> (count code) ptr)
;;                                                (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
;;                                                  (recur ptr stack))
;;                                                stack))))
;;                      (catch Exception e
;;                        (prn "HERE")
;;                        (throw e)))
;;                    (apply str
;;                           (interpose \newline
;;                                      (loop [ptr else
;;                                             stack []]
;;                                        (if (> (count code) ptr)
;;                                          (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
;;                                            (recur ptr stack))
;;                                          stack))))))
;;      (count code)]))


;; (defmethod bc->rb :ifacmpeq [[_ _ label] stack code ptr method]
;;   (let [[else] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)
;;         value1 (peek stack)
;;         stack (pop stack)
;;         value2 (peek stack)
;;         stack (pop stack)]
;;     [(conj stack
;;            (format "(if %s.equal?(%s) then %s else %s end)"
;;                    value1
;;                    value2
;;                    (try
;;                      (apply str (interpose \newline
;;                                            (loop [ptr (inc ptr)
;;                                                   stack []]
;;                                              (if (> (count code) ptr)
;;                                                (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
;;                                                  (recur ptr stack))
;;                                                stack))))
;;                      (catch Exception e
;;                        (prn "HERE")
;;                        (throw e)))
;;                    (apply str
;;                           (interpose \newline
;;                                      (loop [ptr else
;;                                             stack []]
;;                                        (if (> (count code) ptr)
;;                                          (let [[stack ptr] (bc->rb (nth code ptr) stack code ptr method)]
;;                                            (recur ptr stack))
;;                                          stack))))))
;;      (count code)]))

(defmethod bc->rb :goto [[i _ label] stack code ptr method]
  (if (> label ptr)
    (let [[target] (keep-indexed (fn [idx [n]] (when (= n label) idx)) code)]
      [stack target])
    (do
      (doseq [c code]
        (prn c))
      (throw (Exception.)))))

(defprotocol CodeGenerator
  (push-value [cg v])
  (push-code [cg c])
  (peek-value [cg])
  (pop-value [cg])
  (codes [cg])
  (values [cg])
  (drop-code [cg]))

(deftype RubyJVM [code value]
  CodeGenerator
  (push-value [cg v]
    (RubyJVM. code (conj value v)))
  (push-code [cg c]
    (RubyJVM. (conj code c) value))
  (peek-value [cg]
    (peek value))
  (pop-value [cg]
    (RubyJVM. code (pop value)))
  (codes [cg]
    code)
  (values [cg]
    value)
  (drop-code [cg]
    (RubyJVM. [] value)))

(defn rubyjvm []
  (RubyJVM. [] []))

(defn bytecode-to-ruby [method]
  (let [code (first
              (for [attr (:attributes method)
                    :when (= "Code" (:name attr))
                    :let [code (:code (:info attr))]
                    :when (> 35 (count code))]
                code))]
    (loop [ptr 0
           stack (rubyjvm)]
      (if (> (count code) ptr)
        (let [c (nth code ptr)
              [stack ptr] (bc->rb c stack code ptr method)]
          (recur ptr stack))
        (codes stack)))))

(defn ruby-method [method]
  ["  def " (ruby-method-name (:name method)) " ("
   (interpose
    ", "
    (for [attr (:attributes method)
          :when (= "Code" (:name attr))
          attr (:attributes (:info attr))
          :when (= "LocalVariableTable" (:name attr))
          :let [locals (sort-by :index (:info attr))]
          local (take (count (:args method))
                      (if-not ((:access method) :static)
                        (rest locals)
                        locals))]
      (:name local)))
   ")"
   "\n"
   "    __return = nil\n"
   (interpose \newline
              (map
               (partial str "    ")
               (bytecode-to-ruby method)))
   (if (= 'boolean (:return method))
     ["\n    #this isn't right"
      "\n    __return = (__return == 0 or __return == false ? false : true)"]
     [])
   "\n    __return"
   "\n"
   "  end\n"])

(defn ruby [class]
  ["class " (ruby-class-name (:self class))
   "\n"
   (for [field (:fields class)]
     ["  " (if ((:access field) :static)
             "@@"
             "@")
      (:name field) " = nil" "\n"])
   "attr_accessor "
   (apply str (interpose \, (for [field (:fields class)]
                              (str ":" (:name field)))))
   "\n"
   (->> (:methods class)
        (group-by :name)
        (sort-by first)
        (map (fn [[method-name methods]]
               (if (= 1 (count methods))
                 (map ruby-method methods)
                 (let [argcs (for [m methods]
                               (count (:args m)))]
                   (cons
                    ["  def " (ruby-method-name method-name) " (*args)\n"
                     "    case args.length\n"
                     (for [argc (sort argcs)]
                       ["      when " argc "\n"
                        "        " (ruby-method-name (symbol (str (name method-name) "_" argc)))
                        "(*args)\n"])
                     (if (= method-name '<init>)
                       ["    when 0\n"
                        "      nil\n"]
                       [])
                     "    else\n"
                     "      raise \"Unexpected arg count\"\n"
                     "    end"
                     "\n end\n"]
                    (for [m methods]
                      (ruby-method
                       (assoc m :name (symbol (str (name (:name m))
                                                   "_"
                                                   (count (:args m)))))))))))))
   ["  def initialize (*args)\n"
    "    _LT_init_GT_(*args)\n"
    "  end\n"]
   ["  static_init()\n"]
   "end\n"])
