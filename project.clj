(defproject runuo "0.1.0-SNAPSHOT"
  :description "RunUO"
  :url "http://github.com/aamedina/runuo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :warn-on-reflection true
  :min-lein-version "2.0.0"
  :plugins [[lein-clr "0.2.2"]]
  :clr {:cmd-templates
        {:clj-exe   ["mono" ["resources/clojure/1.7" %1]]
         :nuget-ver ["mono" [*PATH "nuget.exe"] "install" %1 "-Version" %2]
         :nuget-any ["mono" [*PATH "nuget.exe"] "install" %1]}
        :main-cmd      [:clj-exe "Clojure.Main.exe"]
        :compile-cmd   [:clj-exe "Clojure.Compile.exe"]}
  :main ^:skip-aot runuo.main)
