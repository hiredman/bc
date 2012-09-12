(ns bc.modifiers)

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
