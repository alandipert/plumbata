(set-env!
 :dependencies (template
                [[org.clojure/clojure ~(clojure-version)]
                 [org.clojure/data.priority-map "0.0.7"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]])
 :source-paths #{"src"})

(require '[adzerk.bootlaces :refer :all])

(def +version+ "0.0.1")
(bootlaces! +version+)
