(defproject shogi "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 ;; Deprecated? [medley "0.6.0"]
                 [cheshire "5.5.0"] ;; JSON encoding and decoding to/from Clojure objects
                 [org.clojure/java.jdbc "0.4.2"] ;; Main library for database usage
                 [org.xerial/sqlite-jdbc "3.8.11"] ;; Specific JDBC driver for SQLite.
                 ;;[lobos "1.0.0-beta3"] ;; SQL database schema manip/migration library
                 [org.postgresql/postgresql "9.4-1203-jdbc4"] ;; PostgreSQL driver
                 ;;[korma "0.4.2"] ;; 'Tasty SQL'-use API.  For easy database integration.
                 ;;[sqlingvo "0.7.15"] ;; PostgreSQL DSL
                 ]
  :main ^:skip-aot shogi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
