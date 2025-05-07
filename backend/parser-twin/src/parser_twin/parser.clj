(ns parser-twin.parser
  (:require [me.raynes.fs :as fs]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [jsonista.core :as j]
            [ring.websocket :as ws])
  (:import (java.io File)
           (org.antlr.v4 Tool)
           (org.antlr.v4.tool ANTLRToolListener ANTLRMessage)
           (org.antlr.v4.runtime CharStream CharStreams CommonTokenStream Token Lexer
                                 Parser ParserRuleContext TokenStream BaseErrorListener
                                 Recognizer CommonToken)
           (java.nio.file Path)
           (org.antlr.v4.runtime.tree TerminalNode ErrorNode)
           (java.io File Reader InputStream)
           (org.stringtemplate.v4 ST)
           (org.antlr.v4.runtime.tree ParseTreeListener)
           (javax.tools ToolProvider StandardJavaFileManager)))

(set! *warn-on-reflection* true)


(def grammar-exts (set (Tool/ALL_GRAMMAR_EXTENSIONS)))
(def ^:private parsers-root (.getCanonicalFile (io/file "")))
(def ^:private parser-root-package "generated")

(def absent-grammar-files-message
  (str "Cannot find grammar files with either " (str/join " or " grammar-exts) " extension."))

(defn- try-copy-additional-java-files [files ^File target-path ^String package]
  (doseq [{:keys [filename tempfile]} files
          :when (= (fs/extension filename) ".java")]
    (with-open [out (->> (File. target-path ^String filename) io/writer)]
      (io/copy (str "package " package ";\n") out)
      (io/copy tempfile out))))

(defn- user-readable-message [^Tool tool ^ANTLRMessage msg]
  (let [err-mgr (.errMgr tool)
        msg-st (.getMessageTemplate err-mgr msg)
        output-msg (.render msg-st)]
    (if (.formatWantsSingleLineMessage err-mgr)
      (clojure.string/replace output-msg #"\n" " ")
      output-msg)))

(defn- ^Tool process-antlr-tool [options]
  (let [errors (atom [])
        warnings (atom [])
        tool (Tool. (into-array String options))]
    (.removeListeners tool)
    (.addListener
     tool
     (proxy [ANTLRToolListener] []
       (error [msg]
         (swap! errors conj (user-readable-message tool msg)))
       (warning [msg]
         (swap! warnings conj (user-readable-message tool msg)))
       (info [msg]
         (println "[info]:" msg))))
    ;; process
    (.processGrammarsOnCommandLine tool)
    ;; return errors
    {:errors @errors
     :warnings @warnings}))

(defn- prepare-grammar-files [files]
  (let [tmp-dir ^File (fs/temp-dir "grammar-files")]
    [tmp-dir
     (for [{:keys [filename tempfile] :as spec}
           (if (map? files) [files] files)
           :let [new-tempfile (File. tmp-dir ^String filename)]]
       (do (io/copy tempfile new-tempfile)
           ;; (io/delete-file tempfile)
           (assoc spec :tempfile new-tempfile)))]))

(defn compile-java-files [files]
  (->>
   (map #(.getPath ^File %) files)
   (filter #(.endsWith ^String % ".java"))
   (list* "-d" "target/classes")
   (into-array String)
   (.run
    (ToolProvider/getSystemJavaCompiler)
    nil
    nil
    nil)
   (zero?)))

(defonce languages (atom {}))

(defn class-array [seq]
  (into-array Class seq))

(defn- register-lang-helper [lang-name ^Class lexer-class ^Class parser-class]
  (let [lang-name (name lang-name)]
    (let [lexer-cnstr (.getDeclaredConstructor lexer-class (class-array [CharStream]))
          parser-cnstr (.getDeclaredConstructor parser-class (class-array [TokenStream]))
          rule-names (-> (.getField parser-class "ruleNames") (.get nil))]
      (->>
       (for [rule-name rule-names
             :let [method (.getMethod parser-class rule-name (class-array []))]]
         [rule-name (fn [parser] (.invoke method parser (object-array [])))])
       (into {})
       (hash-map :make-lexer (fn [stream] (.newInstance lexer-cnstr (object-array [stream])))
                 :make-parser (fn [tokens] (.newInstance parser-cnstr (object-array [tokens])))
                 :rule-parse-fn)
       (swap! languages assoc lang-name)))))

(defn- register-lang [grammar ^File target-dir package]
  (let [{:keys [parser-class lexer-class]}
        (into {}
              (for [^File file (fs/list-dir target-dir)
                    :let [[^String fname ext] (fs/split-ext (.getName file))]
                    :when (= ext ".java")]
                (cond
                  (.endsWith fname "Parser")
                  [:parser-class (str package "." fname)]
                  (.endsWith fname "Lexer")
                  [:lexer-class (str package "." fname)])))]
    (assert lexer-class)
    (assert parser-class)
    (register-lang-helper
     grammar (Class/forName lexer-class) (Class/forName parser-class))))

(defn generate-parser
  [files]
  (let [[tmp-dir files] (prepare-grammar-files files)]
    (try
      (if-let [grammar-files
               (->> files
                    (filter (comp grammar-exts fs/extension :filename))
                    (seq))]
        (let [grammar (str "parser" (System/currentTimeMillis))
              package (str parser-root-package "." grammar)
              target-path ^File (apply fs/file parsers-root (str/split package #"\."))]
          (fs/delete-dir target-path)
          (let [status-map
                (-> ["-package" package "-o" (.getPath target-path)]
                    (into (map #(-> ^File (:tempfile %) .getPath)) grammar-files)
                    (process-antlr-tool))]
            (if (> (count (:errors status-map)) 0)
              {:code 400 :body (j/write-value-as-string status-map)}
              (do (try-copy-additional-java-files files target-path package)
                  (if (compile-java-files (fs/list-dir target-path))
                    (do
                      (register-lang grammar target-path package)
                      {:code 200
                       :body (j/write-value-as-string {:uid grammar})})
                    {:code 500})))))
        {:code 400 :body absent-grammar-files-message})
      (finally
        (fs/delete-dir tmp-dir)))))

(defn handle-generate-parser
  [req]
  (generate-parser
   (get-in req [:params "files"])))

(defprotocol CharStreamBuilder
  (^CharStream make-stream [source] "Create CharStream from `source`"))

(extend-protocol CharStreamBuilder
  File
  (make-stream [source] (CharStreams/fromFileName (.getPath source)))
  Path
  (make-stream [source] (CharStreams/fromPath source))
  Reader
  (make-stream [source] (CharStreams/fromReader source))
  InputStream
  (make-stream [source] (CharStreams/fromStream source))
  String
  (make-stream [source] (CharStreams/fromString source)))

(defn init-parser [lang rule source]
  (let [lang (name lang)]
    (if-let [{:keys [make-lexer make-parser rule-parse-fn]} (@languages lang)]
      (let [lexer (-> source make-stream make-lexer)
            tokens (CommonTokenStream. lexer)
            parser (make-parser tokens)
            rule-name (name rule)]
        (if-let [parse-fn (rule-parse-fn rule-name)]
          [lexer parser (partial parse-fn parser)]
          (throw (IllegalArgumentException.
                  (str "Invalid rule name - " rule-name ".")))))
      (throw (IllegalArgumentException.
              (str "Language '" lang "' isn't registered."))))))

(defn- rule-name [^Parser parser ^ParserRuleContext tree]
  (->> tree .getRuleIndex (aget (.getRuleNames parser))))

(defn seq!!
  "Returns a (blocking!) lazy sequence read from a channel."
  [c]
  (lazy-seq
   (when-let [v (async/<!! c)]
     (cons v (seq!! c)))))

(defn now []
  (System/nanoTime))

(def ^:private nop-timer
  {:stop-fn identity :start-fn identity})

(defn make-timer [parent]
  (let [start (atom nil)
        elapsed-time (atom 0)
        {:keys [start-fn stop-fn]} (or parent nop-timer)]
    {:stop-fn (fn [time]
                (stop-fn time)
                (assert @start)
                (swap! elapsed-time + (- time @@start))
                (reset! start nil))
     :start-fn (fn [time]
                 (start-fn time)
                 (reset! start time))
     :parent parent
     :elapsed-time elapsed-time}))

(defn start-timer [{:keys [start-fn] :as timer}]
  (let [now (delay (now))]
    (start-fn now)
    (force now)))

(defn stop-timer [{:keys [stop-fn] :as timer}]
  (stop-fn (now)))

(defn elapsed-time [{:keys [elapsed-time]}]
  @elapsed-time)

(defn parent-timer [{:keys [parent]}]
  parent)

(defn with-id-generator [id-col]
  (let [counter (atom 0)]
    (fn [& {:as props}]
      (assoc props id-col (swap! counter inc)))))

(defn- init-context []
  (let [current-nodes (atom nil)]
    {:timer (atom nop-timer)
     :make-node (with-id-generator :id)
     :make-step (with-id-generator :step)
     :current-nodes current-nodes
     :current-node-id (fn [] (:id (first @current-nodes)))
     :chan (async/chan)}))

(defn- lexer-error-span [^Lexer lexer & args]
  {:startIndex (._tokenStartCharIndex lexer)
   :stopIndex (.getCharIndex lexer)})

(defn- parser-error-span [^Parser parser ^Token offending-sym & args]
  {:startIndex (.. parser getContext getStart getStartIndex)
   :stopIndex (.getStopIndex offending-sym)})

(defn- configurate-error-listener
  [^Recognizer recognizer error-type
   & {:keys [make-node make-step timer chan current-node-id current-nodes]}]
  (.removeErrorListeners recognizer)
  (let [get-error-span
        (condp instance? recognizer
          Lexer lexer-error-span
          Parser parser-error-span)]
    (.addErrorListener
     recognizer
     (proxy [BaseErrorListener] []
       (syntaxError [recognizer sym line col msg e]
         (stop-timer @timer)
         (->> (make-node
               (assoc
                (get-error-span recognizer sym)
                :type error-type
                :parent (current-node-id)
                :message msg))
              (make-step)
              (async/>!! chan))
         (start-timer @timer))))))

(defn- token-name [^Token token ^Lexer lexer]
  (let [ttype (.getType token)
        vocab (.getVocabulary lexer)]
    (or (.getSymbolicName vocab ttype)
        (.getDisplayName vocab ttype)
        ;; (.getText token)
        )))

(defn- visit-terminal
  [lexer ^TerminalNode node target-type
   & {:keys [make-node make-step timer chan current-node-id]}]
  (stop-timer @timer)
  (let [token (.getSymbol node)]
    (->>
     (make-node
      :type target-type
      :parent (current-node-id)
      :name (token-name token lexer)
      :value (.getText token)
      :startIndex (.getStartIndex token)
      :stopIndex (.getStopIndex token))
     (make-step)
     (async/>!! chan)))
  (start-timer @timer))

(defn- run-parser [parse-fn chan]
  (future
    (try
      (parse-fn)
      (catch Throwable e
        (println "Parsing failed due to internal error.\n" e))
      (finally
        (async/close! chan))))
  chan)

(defn parse-source [lang rule source]
  (let [[^Lexer lexer ^Parser parser parse-fn]
        (init-parser lang rule source)
        {:keys [timer make-node make-step current-nodes current-node-id chan]
         :as context}
        (init-context)]
    ;; configurate error listeners
    (configurate-error-listener lexer :lexerError context)
    (configurate-error-listener parser :parserError context)
    ;; add parser listener
    (.addParseListener
     parser
     (proxy [ParseTreeListener] []
       (enterEveryRule [ctx]
         (stop-timer @timer)
         (let [node
               (make-node
                :type :node
                :parent (current-node-id)
                :value (rule-name parser ctx)
                :processed false)]
           (async/>!! chan (make-step node))
           (swap! current-nodes conj node)
           (swap! timer make-timer)
           (start-timer @timer)))
       (exitEveryRule [ctx]
         (stop-timer @timer)
         (->> {:id (current-node-id)
               :elapsedTime (elapsed-time @timer)
               :processed true}
              (make-step)
              (async/>!! chan))
         (swap! current-nodes rest)
         (swap! timer parent-timer)
         (start-timer @timer))
       (visitTerminal [^TerminalNode node]
         (visit-terminal lexer node :terminalNode context))
       (visitErrorNode [^ErrorNode node]
         (visit-terminal lexer node :errorNode context))))
    ;; async parser
    (run-parser parse-fn chan)))

(defonce channels {})

(defn handle-parse-req [req]
  (assert (ws/upgrade-request? req))
  (let [counter (atom 0)]
    {::ws/listener
     {:on-open
      (fn [socket]
        (ws/send socket "Connected!"))
      :on-message
      (fn [socket event]
        (cond
          (= event "exit")
          (ws/close socket)
          (= event "inc")
          (ws/send socket (str (swap! counter inc)))
          :else (ws/send socket event)))}}))

