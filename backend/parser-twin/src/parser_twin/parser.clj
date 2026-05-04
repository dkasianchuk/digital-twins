(ns parser-twin.parser
  (:require [me.raynes.fs :as fs]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [jsonista.core :as j]
            [ring.websocket :as ws]
            [clojure.string :as string]
            [clojure.stacktrace :as stacktrace])
  (:import (java.io File Reader InputStream)
           (java.util Map HashMap)
           (org.antlr.v4 Tool)
           (org.antlr.v4.tool ANTLRToolListener ANTLRMessage)
           (org.antlr.v4.runtime CharStream CharStreams CommonTokenStream Token Lexer
                                 Parser ParserRuleContext TokenStream BaseErrorListener
                                 Recognizer CommonToken)
           (java.nio.file Path)
           (org.antlr.v4.runtime.tree TerminalNode ErrorNode)
           (org.stringtemplate.v4 ST)
           (org.antlr.v4.runtime.tree ParseTreeListener)
           (javax.tools ToolProvider)))

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
      (string/replace output-msg #"\n" " ")
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

(defn- timed-lexer-class-impl [package lexer-class]
  (let [timed-lexer-class (str lexer-class "Timed")]
    {:code
     (format
      "
package %s;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.CharStream;
import java.util.Map;

public class %s extends %s {
  private Map<Token, Long> durations;
  public %s(CharStream input, Map<Token, Long> durations) {
    super(input);
    this.durations = durations;
  }

  @Override
  public Token nextToken() {
    long start = System.nanoTime();
    Token token = super.nextToken();
    long duration = System.nanoTime() - start;

    this.durations.put(token, duration);

    return token;
  }
}"
      package timed-lexer-class lexer-class timed-lexer-class)
     :lexer-class timed-lexer-class}))

(defn- prepare-java-files [^File target-dir package]
  (let [{:keys [parser-class lexer-class]}
        (into {}
              (for [^File file (fs/list-dir target-dir)
                    :let [[^String fname ext] (fs/split-ext (.getName file))]
                    :when (= ext ".java")]
                (cond
                  (.endsWith fname "Parser") [:parser-class fname]
                  (.endsWith fname "Lexer") [:lexer-class fname])))]
    (assert lexer-class)
    (assert parser-class)
    (let [{:keys [code lexer-class]} (timed-lexer-class-impl package lexer-class)]
      ;; generate timed lexer class
      (io/copy
       code
       (File. target-dir (str lexer-class ".java")))
      ;; return parser and lexer classes
      {:lexer-class (str package "." lexer-class)
       :parser-class (str package "." parser-class)})))

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

(defn- register-lang [^String lang ^String lexer-class ^String parser-class]
  (let [lexer-class (Class/forName lexer-class)
        parser-class (Class/forName parser-class)
        lexer-cnstr (.getDeclaredConstructor lexer-class (class-array [CharStream Map]))
        parser-cnstr (.getDeclaredConstructor parser-class (class-array [TokenStream]))
        rule-names (-> (.getField parser-class "ruleNames") (.get nil))]
    (->>
     (for [rule-name rule-names
           :let [method (.getMethod parser-class rule-name (class-array []))]]
       [rule-name (fn [parser] (.invoke method parser (object-array [])))])
     (into {})
     (hash-map
      :make-lexer (fn [stream durations]
                    (.newInstance lexer-cnstr (object-array [stream durations])))
      :make-parser (fn [tokens]
                     (.newInstance parser-cnstr (object-array [tokens])))
      :rule-parse-fn)
     (swap! languages assoc lang))))

(comment
  (generate-parser
   [{:filename "test.g4"
     :tempfile (java.io.File. "resources/grammars/test/test.g4")}])
  (generate-parser
   [{:filename "calculator.g4"
     :tempfile (java.io.File. "resources/grammars/calculator/calculator.g4")}]))

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
                  (let [{:keys [lexer-class parser-class]}
                        (prepare-java-files target-path package)]
                    (if (compile-java-files (fs/list-dir target-path))
                      (do
                        (register-lang grammar lexer-class parser-class)
                        {:code 200
                         :body (j/write-value-as-string {:lang grammar})})
                      {:code 500}))))))
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
      (let [token-durations (HashMap.)
            lexer (-> source make-stream (make-lexer token-durations))
            tokens (CommonTokenStream. lexer)
            parser (make-parser tokens)
            rule-name (name rule)]
        (if-let [parse-fn (rule-parse-fn rule-name)]
          {:lexer lexer
           :parser parser
           :parse-fn (partial parse-fn parser)
           :token-durations token-durations}
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

(defn- nanos->timestamp [nanos]
  (let [total-seconds (quot nanos 1000000000)
        minutes (quot total-seconds 60)
        seconds (mod total-seconds 60)
        nano-frac (mod nanos 1000000000)]
    (format "%02d:%02d.%09d" minutes seconds nano-frac)))

(defn parent-timer [{:keys [parent]}]
  parent)

(defn with-id-generator [id-col]
  (let [counter (atom 0)]
    (fn [& {:as props}]
      (assoc props id-col (swap! counter inc)))))

(defn- init-context [& {:keys [token-durations]}]
  (let [current-nodes (atom nil)]
    {:timer (atom nop-timer)
     :make-node (with-id-generator :id)
     :make-step (with-id-generator :step)
     :current-nodes current-nodes
     :current-node-id (fn [] (:id (first @current-nodes)))
     :chan (async/chan)
     :token-durations token-durations}))

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
   & {:keys [make-node make-step timer chan current-node-id ^Map token-durations]}]
  (stop-timer @timer)
  (let [token (.getSymbol node)]
    (->>
     (make-node
      :type target-type
      :parent (current-node-id)
      :name (token-name token lexer)
      :value (.getText token)
      :startIndex (.getStartIndex token)
      :stopIndex (.getStopIndex token)
      :elapsedTime (nanos->timestamp (.get token-durations token))
      :processed true)
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
  (let [{:keys [^Lexer lexer ^Parser parser parse-fn] :as props}
        (init-parser lang rule source)
        {:keys [timer make-node make-step current-nodes current-node-id chan]
         :as context}
        (init-context props)]
    ;; configurate error listeners
    (configurate-error-listener lexer :lexerError context)
    (configurate-error-listener parser :parserError context)
    ;; do not build parse tree
    (.setBuildParseTree parser false)
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
               :elapsedTime (nanos->timestamp (elapsed-time @timer))
               :processed true}
              (make-step)
              (async/>!! chan))
         (swap! current-nodes pop)
         (swap! timer parent-timer)
         (start-timer @timer))
       (visitTerminal [^TerminalNode node]
         (visit-terminal lexer node :terminalNode context))
       (visitErrorNode [^ErrorNode node]
         (visit-terminal lexer node :errorNode context))))
    ;; async parser
    (run-parser parse-fn chan)))

(defonce channels (atom {}))

(defn- ws-send-json [socket data & args]
  (apply ws/send socket (j/write-value-as-string data) args))

(defmulti process-event (fn [event & args] (:type event)))

(defn- validate-event [event required-keys socket]
  (if-let [missing-keys
           (->>
            (remove #(contains? event %) required-keys)
            (seq))]
    (do
      (ws-send-json
       socket
       {:type "error"
        :message (str "Missing required params: "
                      (string/join \, (map name missing-keys)))})
      nil)
    event))

(defmethod process-event "init"
  [{:keys [lang rule source] :as event} socket channel]
  (when (validate-event event [:lang :rule :source] socket)
    (let [uuid (str (random-uuid))
          chan (parse-source lang rule source)]
      (swap! channels assoc uuid chan)
      (reset! channel chan)
      (ws-send-json socket {:uuid uuid}))))

(defmethod process-event "set"
  [{:keys [uuid] :as event} socket channel]
  (when (validate-event event [:uuid] socket)
    (if-let [chan (@channels uuid)]
      (do
        (reset! channel chan)
        (ws-send-json socket {:uuid uuid}))
      (ws-send-json
       socket
       {:type "error"
        :message "Missing parser"}))))

(defn- step-helper [pred count timeout channel socket]
  (let [sleep-fn
        (if timeout
          #(Thread/sleep ^long timeout)
          (constantly nil))]
    (locking @channel
      (loop [left count]
        (if (zero? left)
          (ws-send-json socket {:type "stepEnd"})
          (if-let [value (async/<!! @channel)]
            (do
              (ws-send-json socket value)
              (sleep-fn)
              (recur (cond-> left (pred value) dec)))
            (ws-send-json socket {:type "end"})))))))

(defmethod process-event "simpleStep"
  [{:keys [count timeout] :as event} socket channel]
  (when (validate-event event [:count] socket)
    (step-helper (constantly true) count timeout channel socket)))

(defmethod process-event "tokenStep"
  [{:keys [count timeout] :as event} socket channel]
  (when (validate-event event [:count] socket)
    (step-helper
     (comp #{:terminalNode :errorNode} :type) count timeout channel socket)))

(defmethod process-event "completeNodeStep"
  [{:keys [id timeout] :as event} socket channel]
  (when (validate-event event [:id] socket)
    (step-helper (every-pred :processed (comp #{id} :id)) 1 timeout channel socket)))

(defmethod process-event "exit"
  [event socket channel]
  (ws/close socket))

(defn- make-pinger [socket]
  (doto
      (Thread.
       (fn []
         (while (ws/open? socket)
           (ws/ping socket)
           ;; 5 secs
           (Thread/sleep (* 1000 5)))))
    (.start)))

(defn handle-parse-req [req]
  (assert (ws/upgrade-request? req))
  (let [curr-channel (atom nil)
        pinger (atom nil)]
    {::ws/listener
     {:on-open
      (fn [socket]
        (println "Socket opened!")
        (reset! pinger (make-pinger socket)))
      :on-message
      (fn [socket event]
        (println "event -> " event)
        (try
          (when-let [event
                     (-> event
                         j/read-value
                         (update-keys keyword)
                         (validate-event [:type] socket))]
            (process-event event socket curr-channel))
          (catch Exception e
            (println "Error in socket.\n")
            (stacktrace/print-stack-trace e 10)
            (flush)
            (ws-send-json socket {:type "error" :message "Internal Error"}))))
      :on-close
      (fn [socket]
        (println "Socket closed!")
        (.stop ^Thread @pinger))}}))

(declare make-node)

(defn chan-from-events [events]
  (let [chan (async/chan)]
    (future
      (doseq [event events]
        (println "event->" event)
        (async/>!! chan event)))
    chan))

(defn make-children [chan]
  (lazy-seq
   (let [event (async/<!! chan)]
     (when-not (= (:type event) :exitRule)
       (cons (make-node chan event) (make-children chan))))))

(defn make-node
  ([chan]
   (make-node chan (async/<!! chan)))
  ([chan event]
   (case (:type event)
     :enterRule (cons (:ruleName event) (make-children chan))
     :visitTerminal event
     :visitErrorNode event)))

(defn make-test-listener
  [events
   & {:keys [enter-rule
             exit-rule
             visit-terminal
             visit-error-node]}]
  (future
    (doseq [{:keys [type] :as event} events]
      (println "event->" event)
      (case type
        :enterRule (enter-rule event)
        :exitRule (exit-rule event)
        :visitTerminal (visit-terminal event)
        :visitErrorNode (visit-error-node event)))))

(defn make-test-parser
  [events]
  (let [chan (async/chan)]
    ;; configurate listener
    (make-test-listener
     events
     :enter-rule (fn [event] (async/>!! chan (make-node-fn chan event)))
     :exit-rule (fn [event] (async/>!! chan ::stop))
     :visit-terminal (fn [event]
                       (async/>!!
                        chan
                        (assoc event
                               :span (atom
                                      (select-keys event [:startIndex :stopIndex])))))
     :visit-error-node (fn [event] (async/>!! chan event)))
    ;; return tree
    (async/<!! chan)))

;; (defprotocol LazyNode
;;   (force! [this])
;;   (skip! [this]))

;; (extend-protocol LazyNode
;;   clojure.lang.ISeq
;;   (force! [node]
;;     (doseq [child (rest node)]
;;       (force! child))
;;     node)
;;   java.lang.Object
;;   (force! [node]
;;     node))

(defn force! [node]
  (if-let [children (:children node)]
    (do
      (doseq [child children]
        (force! child))
      node)
    node))

(defn constantly-nil []
  nil)

(defn make-children-fn [chan span]
  (letfn [(set-span [key-name node]
            (swap! span assoc key-name (-> node :span deref (get key-name))))
          (make-children [prev-node]
            (let [node (async/<!! chan)]
              (if (identical? node ::stop)
                (do (set-span :stopIndex prev-node) nil)
                (cons
                 node
                 (lazy-seq
                  (force! node)
                  (make-children node))))))]
    (lazy-seq
     (let [first-node (async/<!! chan)]
       (cons
        first-node
        (lazy-seq
         (force! first-node)
         (set-span :startIndex first-node)
         (make-children first-node)))))))

(defn make-node-fn
  ([chan event]
   (let [span (atom {})]
     {:head (:ruleName event)
      :children (make-children-fn chan span)
      :span span}))
  ([chan parser ctx]
   (let [span (atom {})]
     {:head (rule-name parser ctx)
      :children (make-children-fn chan span)
      :span span})))

(defn- visit-terminal-fn
  [lexer ^TerminalNode node target-type chan]
  (let [token (.getSymbol node)]
    (->>
     {:type target-type
      :name (token-name token lexer)
      :value (.getText token)
      :span (atom
             {:startIndex (.getStartIndex token)
              :stopIndex (.getStopIndex token)})}
     (async/>!! chan))))

(defn parse-source-lazy [lang rule source & {:keys [buffer-size] :or {buffer-size 2}}]
  (let [{:keys [^Lexer lexer ^Parser parser parse-fn] :as props}
        (init-parser lang rule source)
        chan (async/chan buffer-size)]
    ;; configurate error listeners
    ;; (configurate-error-listener lexer :lexerError context)
    ;; (configurate-error-listener parser :parserError context)
    ;; do not build parse tree
    (.setBuildParseTree parser false)
    (.removeParseListeners parser)
    ;; add parser listener
    (.addParseListener
     parser
     (proxy [ParseTreeListener] []
       (enterEveryRule [ctx]
         (println (str "enterEveryRule->" (rule-name parser ctx)))
         (async/>!! chan (make-node-fn chan parser ctx)))
       (exitEveryRule [^ParserRuleContext ctx]
         (println (str "exitEveryRule->" (rule-name parser ctx)))
         (async/>!! chan ::stop))
       (visitTerminal [^TerminalNode node]
         (println (str "visitTerminal->" node))
         (visit-terminal-fn lexer node :terminalNode chan))
       (visitErrorNode [^ErrorNode node]
         (println (str "visitErrorNode->" node))
         (visit-terminal-fn lexer node :errorNode chan))))
    ;; async parser
    (async/<!! (run-parser parse-fn chan))))

(defn test-parse-source-lazy []
  (let [tree
        (parse-source-lazy
         "parser1777893995512"
         :expression
         "a + b")]
    ;; (println "last child->" (last (:children tree)))
    ;; (force! tree)
    tree))

(defn test-lazy-tree []
  (let [events
        [{:type :enterRule
          :ruleName "*"}
         {:type :enterRule
          :ruleName "+"}
         {:type :enterRule
          :ruleName "+"}
         {:type :visitTerminal
          :value "a1"
          :startIndex 0
          :stopIndex 0}
         {:type :visitTerminal
          :value "a2"
          :startIndex 1
          :stopIndex 1}
         {:type :exitRule}
         {:type :enterRule
          :ruleName "+"
          :startIndex 2
          :stopIndex 2}
         {:type :visitTerminal
          :value "b1"
          :startIndex 3
          :stopIndex 3}
         {:type :visitTerminal
          :value "b2"
          :startIndex 4
          :stopIndex 4}
         {:type :exitRule}
         {:type :exitRule}
         {:type :enterRule
          :ruleName "-"
          :startIndex 5
          :stopIndex 5}
         {:type :visitTerminal
          :value "c1"
          :startIndex 6
          :stopIndex 6}
         {:type :visitTerminal
          :value "c2"
          :startIndex 7
          :stopIndex 7}
         {:type :exitRule}
         {:type :enterRule
          :ruleName "+"
          :startIndex 8
          :stopIndex 8}
         {:type :visitTerminal
          :value "d1"
          :startIndex 9
          :stopIndex 9}
         {:type :visitTerminal
          :value "d2"
          :startIndex 10
          :stopIndex 10}
         {:type :exitRule}
         {:type :exitRule}]
        tree (make-test-parser events)]
    tree
    ;; (force! tree)
    ;; true
    ;; (println (str "second-> " (nth tree 2)))
    ;; tree
    ))
