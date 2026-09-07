;;; gptel-web-tools-bridge.el --- Route gptel web tools through an MCP provider -*- lexical-binding: t; -*-

;; Copyright (C) 2026 mclbn

;; Author: mclbn
;; URL: https://github.com/mclbn/gptel-web-tools-bridge
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (gptel "0.9.9") (mcp "0.1.0"))
;; Keywords: convenience, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package registers two gptel tools, `web_search' and `web_fetch'
;; (category "web"), and routes them through a Model Context Protocol
;; search provider.  The provider is chosen at runtime with `M-x
;; gptel-web-tools-bridge-set-provider' or by setting
;; `gptel-web-tools-bridge-provider':
;;
;;   exa      Exa's hosted MCP server (web_search_exa, web_fetch_exa)
;;   searxng  an mcp-searxng instance (searxng_web_search, web_url_read)
;;   nil      no bridge; the Emacs web browser (eww) serves both tools
;;
;; A call never fails outright.  When the provider is not connected,
;; reports an error, times out, or returns something unparseable, the call
;; falls back to the eww implementations in `gptel-agent-tools'.  Every
;; result is prefixed with a single "Source:" line naming whatever
;; actually served it, so a silent fallback is visible both in the chat
;; buffer and to the model.
;;
;; Adding a provider means adding an entry to
;; `gptel-web-tools-bridge-providers': a server name, two tool names, and
;; two functions building each tool's argument plist.  Nothing else in
;; this file is provider-specific.
;;
;; Requirements and caveats:
;;
;; - `gptel-agent' supplies the native implementations
;;   (`gptel-agent--web-search-eww' and `gptel-agent--read-url').  It is
;;   loaded on demand, the first time a call is served natively, so it
;;   need not be loaded at startup.  Without it the bridge still works,
;;   but native calls return an error string instead of results.
;;
;; - IMPORTANT: mcp.el's `mcp-async-call-tool' passes :timeout to jsonrpc
;;   but no :timeout-fn, so a tools/call that times out invokes neither
;;   callback.  The fallback never runs and gptel waits forever.  Advise
;;   `mcp-async-call-tool' to supply a :timeout-fn calling the error
;;   callback; see the README.  Everything else here degrades gracefully,
;;   but a timeout cannot be detected without that advice.
;;
;; - The hosted Exa server must expose both tools.  Appending ?tools= to
;;   its URL replaces the defaults, so list both explicitly:
;;   https://mcp.exa.ai/mcp?tools=web_search_exa,web_fetch_exa
;;   Exa's own per-tool timeout is 60s, so give the server a client
;;   :timeout above that (90 works) or slow crawls fall back needlessly.

;;; Code:

(require 'gptel)
(require 'mcp)
(require 'mcp-hub)
(require 'subr-x)
(require 'url-parse)

(declare-function gptel-agent--web-search-eww "gptel-agent-tools"
                  (tool-cb query &optional count))
(declare-function gptel-agent--read-url "gptel-agent-tools" (tool-cb url))


;;;; Options

(defgroup gptel-web-tools-bridge nil
  "Route gptel's web tools through an MCP search provider."
  :group 'gptel
  :prefix "gptel-web-tools-bridge-")

(defcustom gptel-web-tools-bridge-provider 'exa
  "Provider backing the `web_search' and `web_fetch' gptel tools.

The value is a key in `gptel-web-tools-bridge-providers', or nil to
use the Emacs web browser (eww) directly.  An unknown key behaves
like nil, and says so in each result's \"Source:\" line.

Changing this outside of `gptel-web-tools-bridge-set-provider' leaves
the tools' descriptions describing the previous provider until
`gptel-web-tools-bridge-register-tools' runs again."
  :type '(choice (const :tag "Exa" exa)
                 (const :tag "SearXNG" searxng)
                 (const :tag "No bridge (eww)" nil)
                 (symbol :tag "Other provider key")))

(defcustom gptel-web-tools-bridge-max-characters 8000
  "Maximum number of characters `web_fetch' requests per page.

Only providers that accept a length limit honour this; Exa does (its
own default is a rather small 3000), mcp-searxng does not."
  :type 'natnum)

(defcustom gptel-web-tools-bridge-default-results 5
  "Number of search results requested when the model omits `count'."
  :type 'natnum)

(defcustom gptel-web-tools-bridge-warn-interval nil
  "Minimum number of seconds between fallback warnings.
When nil, warn on every fallback."
  :type '(choice (const :tag "Warn every time" nil)
                 (number :tag "Seconds")))

(defcustom gptel-web-tools-bridge-native-url-predicate
  #'gptel-web-tools-bridge-unroutable-url-p
  "Predicate deciding whether `web_fetch' should bypass the provider.

Called with one argument, the URL string, and should return non-nil
when the URL must be fetched locally instead.  A provider entry can
override this with its own `:native-url-predicate'.  Set to `ignore'
to always use the provider."
  :type 'function)

(defcustom gptel-web-tools-bridge-connect-on-demand t
  "Whether a call may start the provider's MCP server.

When non-nil and the server is not connected, the call is served
natively and a connection attempt is started in the background, so
that later calls reach the provider.  Attempts are throttled to one
per minute."
  :type 'boolean)

(defcustom gptel-web-tools-bridge-override-agent-tools nil
  "Whether to point gptel-agent's own web tools at this bridge.

`gptel-agent' ships `WebSearch' and `WebFetch', and its agent
definitions name those tools in their front matter.  When non-nil,
both are re-registered against this bridge once `gptel-agent-tools'
loads.  Changing this later takes effect on the next call to
`gptel-web-tools-bridge-install-agent-overrides'."
  :type 'boolean)


;;;; Provider argument builders

(defun gptel-web-tools-bridge--exa-search-args (query count)
  "Return the `web_search_exa' argument plist for QUERY and COUNT."
  (list :query query :numResults count))

(defun gptel-web-tools-bridge--exa-fetch-args (url)
  "Return the `web_fetch_exa' argument plist for URL.
Exa's `urls' parameter is an array, and `json-serialize' only treats
vectors as JSON arrays, so URL is wrapped in a vector rather than a
list."
  (list :urls (vector url)
        :maxCharacters gptel-web-tools-bridge-max-characters))

(defun gptel-web-tools-bridge--searxng-search-args (query count)
  "Return the `searxng_web_search' argument plist for QUERY and COUNT.
The tool caps `num_results' at 20."
  (list :query query :num_results (max 1 (min count 20))))

(defun gptel-web-tools-bridge--searxng-fetch-args (url)
  "Return the `web_url_read' argument plist for URL."
  (list :url url))


;;;; Provider table

(defcustom gptel-web-tools-bridge-providers
  '((exa
     :label "exa"
     :server "exa"
     :search-tool "web_search_exa"
     :search-args gptel-web-tools-bridge--exa-search-args
     :fetch-tool "web_fetch_exa"
     :fetch-args gptel-web-tools-bridge--exa-fetch-args
     :search-doc "Served by Exa.  Phrase the query as a description of the \
ideal page rather than as keywords: \"blog post comparing React and Vue \
performance\" beats \"React vs Vue\".  Prefixing the query with \
\"category:company\" or \"category:people\" restricts results to company or \
person profiles.  Excerpts are relevance-selected highlights, not \
necessarily the opening of the page."
     :fetch-doc "Served by Exa, which returns the page as clean markdown \
truncated to a fixed maximum length, so treat a long page as an extract.  \
Exa fetches from its own infrastructure: it cannot read pages that require \
authentication, and URLs on the local machine or a private network are \
fetched locally instead.")
    (searxng
     :label "searxng"
     :server "searxng"
     :search-tool "searxng_web_search"
     :search-args gptel-web-tools-bridge--searxng-search-args
     :fetch-tool "web_url_read"
     :fetch-args gptel-web-tools-bridge--searxng-fetch-args
     :native-url-predicate gptel-web-tools-bridge-loopback-url-p
     :search-doc "Served by a SearXNG instance aggregating conventional \
search engines.  Keyword-style queries work best.  Excerpts are the short \
snippets the engines return."
     :fetch-doc "Served by mcp-searxng, which converts HTML to markdown and \
can extract text from PDFs; binary, media and archive downloads are \
rejected.  URLs on the local machine are fetched locally instead."))
  "Alist of providers usable by `gptel-web-tools-bridge-provider'.

Each element is (KEY . PLIST).  PLIST keys:

`:label'                 short name used in \"Source:\" lines
`:server'                server name in `mcp-hub-servers'
`:search-tool'           MCP tool name for searching
`:search-args'           function of (QUERY COUNT) returning an
                         argument plist for `:search-tool'
`:fetch-tool'            MCP tool name for fetching a URL
`:fetch-args'            function of (URL) returning an argument
                         plist for `:fetch-tool'
`:search-doc'            provider-specific paragraph appended to
                         the `web_search' tool description
`:fetch-doc'             likewise for `web_fetch'
`:native-url-predicate'  optional, overrides
                         `gptel-web-tools-bridge-native-url-predicate'"
  :type '(alist :key-type symbol :value-type sexp))


;;;; URL predicates

(defun gptel-web-tools-bridge--url-parts (url)
  "Return (TYPE . HOST) for URL, both downcased strings.
A URL with no scheme is parsed as if it were https."
  (let* ((url (string-trim (or url "")))
         (absolute (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)
                       url
                     (concat "https://" url)))
         (parsed (url-generic-parse-url absolute))
         (host (or (url-host parsed) "")))
    (cons (downcase (or (url-type parsed) ""))
          ;; url-host keeps the brackets on IPv6 literals.
          (downcase (string-trim host "\\[" "\\]")))))

(defun gptel-web-tools-bridge-loopback-url-p (url)
  "Return non-nil if URL is not HTTP(S), or points at this machine.
Suitable for providers running elsewhere on the local network, which
can resolve private addresses but for which \"localhost\" means their
own host rather than Emacs's."
  (pcase-let ((`(,type . ,host) (gptel-web-tools-bridge--url-parts url)))
    (or (not (member type '("http" "https")))
        (string-empty-p host)
        (member host '("localhost" "0.0.0.0" "::1" "::"))
        (string-match-p "\\`127\\." host)
        (string-suffix-p ".localhost" host))))

(defun gptel-web-tools-bridge-unroutable-url-p (url)
  "Return non-nil if URL cannot be reached from outside this network.
Covers `gptel-web-tools-bridge-loopback-url-p' plus private and
link-local address ranges, unqualified host names, and the usual
local-network suffixes.  Fetching these through a hosted provider
cannot work, and leaks an internal name on the way to failing."
  (or (gptel-web-tools-bridge-loopback-url-p url)
      (pcase-let ((`(,_type . ,host) (gptel-web-tools-bridge--url-parts url)))
        (or (string-match-p "\\`10\\." host)
            (string-match-p "\\`192\\.168\\." host)
            (string-match-p "\\`172\\.\\(1[6-9]\\|2[0-9]\\|3[01]\\)\\." host)
            (string-match-p "\\`169\\.254\\." host)
            (string-match-p "\\`100\\.\\(6[4-9]\\|[7-9][0-9]\\|1[01][0-9]\\|12[0-7]\\)\\."
                            host)
            ;; IPv6 unique-local (fc00::/7) and link-local (fe80::/10).
            (string-match-p "\\`f[cd][0-9a-f][0-9a-f]:" host)
            (string-match-p "\\`fe[89ab][0-9a-f]:" host)
            (string-match-p "\\.\\(local\\|lan\\|internal\\|intranet\\|home\\.arpa\\)\\'"
                            host)
            ;; Unqualified name: no dot at all.
            (not (string-match-p "\\." host))))))

(defun gptel-web-tools-bridge--fetch-natively-p (url plist)
  "Return non-nil if URL should bypass the provider described by PLIST."
  (let ((predicate (or (plist-get plist :native-url-predicate)
                       gptel-web-tools-bridge-native-url-predicate)))
    (and (functionp predicate)
         (condition-case nil
             (funcall predicate url)
           (error t)))))


;;;; Provider plumbing

(defvar gptel-web-tools-bridge--last-warn 0.0
  "Time of the last fallback warning, for throttling.")

(defvar gptel-web-tools-bridge--last-connect 0.0
  "Time of the last on-demand connection attempt, for throttling.")

(defun gptel-web-tools-bridge--provider ()
  "Return the plist for the active provider, or nil in native mode."
  (when gptel-web-tools-bridge-provider
    (alist-get gptel-web-tools-bridge-provider
               gptel-web-tools-bridge-providers)))

(defun gptel-web-tools-bridge--native-note ()
  "Return the \"Source:\" note explaining why no provider is in use."
  (if (null gptel-web-tools-bridge-provider)
      "no provider"
    (format "unknown provider `%s'" gptel-web-tools-bridge-provider)))

(defun gptel-web-tools-bridge--connection (server)
  "Return SERVER's MCP connection if it is connected, else nil."
  (and (featurep 'mcp)
       (hash-table-p mcp-server-connections)
       (let ((connection (gethash server mcp-server-connections)))
         (and connection
              (eq (mcp--status connection) 'connected)
              connection))))

(defun gptel-web-tools-bridge--oneline (string &optional limit)
  "Collapse whitespace in STRING and truncate it to LIMIT characters."
  (let ((flat (string-trim (replace-regexp-in-string
                            "[ \t\n\r]+" " " (or string ""))))
        (limit (or limit 160)))
    (if (> (length flat) limit)
        (concat (substring flat 0 limit) "...")
      flat)))

(defun gptel-web-tools-bridge--stamp (source &optional note)
  "Return a provenance header naming SOURCE, with an optional NOTE."
  (let ((note (gptel-web-tools-bridge--oneline note)))
    (concat "Source: " source
            (if (string-empty-p note) "" (format " (%s)" note))
            "\n\n")))

(defun gptel-web-tools-bridge--warn (label provider reason)
  "Warn that PROVIDER could not serve LABEL because of REASON."
  (let ((now (float-time)))
    (when (or (null gptel-web-tools-bridge-warn-interval)
              (>= (- now gptel-web-tools-bridge--last-warn)
                  gptel-web-tools-bridge-warn-interval))
      (setq gptel-web-tools-bridge--last-warn now)
      (message "%s: %s unavailable (%s); served by eww instead"
               label provider (gptel-web-tools-bridge--oneline reason)))))

(defun gptel-web-tools-bridge--result-error-p (result)
  "Return non-nil if MCP tool RESULT reports an error."
  (let ((error-flag (plist-get result :isError)))
    (and error-flag (not (memq error-flag '(:false :json-false))))))

(defun gptel-web-tools-bridge--parse-result (result)
  "Return the concatenated text blocks of MCP tool RESULT.
RESULT's :content is a vector of blocks when it comes from jsonrpc,
but a list is accepted too."
  (let (texts)
    (mapc (lambda (block)
            (when (equal "text" (plist-get block :type))
              (push (or (plist-get block :text) "") texts)))
          (plist-get result :content))
    (mapconcat #'identity (nreverse texts) "\n")))

(defun gptel-web-tools-bridge--result-count (count)
  "Return a usable result count from COUNT, which may be nil or a string."
  (let ((n (cond ((natnump count) count)
                 ((numberp count) (truncate count))
                 ((and (stringp count)
                       (string-match-p "\\`[0-9]+\\'" (string-trim count)))
                  (string-to-number (string-trim count))))))
    (if (and n (> n 0)) n gptel-web-tools-bridge-default-results)))

(defun gptel-web-tools-bridge--maybe-connect (server)
  "Start SERVER in the background if allowed.
Return non-nil if an attempt was made."
  (when (and gptel-web-tools-bridge-connect-on-demand
             (assoc server mcp-hub-servers)
             (> (- (float-time) gptel-web-tools-bridge--last-connect) 60))
    (gptel-web-tools-bridge-ensure-server)
    t))

(defun gptel-web-tools-bridge--call (label plist tool args callback fallback)
  "Call MCP TOOL with ARGS on the server described by PLIST.

LABEL names the gptel tool in warnings.  CALLBACK is gptel's tool
callback, invoked with the stamped result text.  FALLBACK is called
with a note string when the provider cannot serve the call."
  (let* ((provider (plist-get plist :label))
         (server (plist-get plist :server))
         ;; CALLBACK must run exactly once: gptel counts outstanding tool
         ;; calls, so a second invocation corrupts the count and a missing
         ;; one wedges the request.
         (done nil)
         (bail
          (lambda (reason)
            (unless done
              (setq done t)
              (gptel-web-tools-bridge--warn label provider reason)
              (funcall fallback (format "%s: %s" provider reason)))))
         (succeed
          (lambda (text)
            (unless done
              (setq done t)
              (funcall callback
                       (concat (gptel-web-tools-bridge--stamp provider tool)
                               (if (string-empty-p (string-trim text))
                                   "The provider returned no content."
                                 text))))))
         ;; Only the synchronous half is guarded, and its failure is
         ;; reported after the handler returns: bailing from inside the
         ;; handler would mean an error raised by FALLBACK itself re-entered
         ;; the handler and ran FALLBACK a second time.
         (reason
          (condition-case err
              (if-let* ((connection (gptel-web-tools-bridge--connection server)))
                  (progn
                    (mcp-async-call-tool
                     connection tool args
                     (lambda (result)
                       (let* ((parsed (condition-case nil
                                          (gptel-web-tools-bridge--parse-result
                                           result)
                                        (error nil)))
                              (blank (or (null parsed)
                                         (string-empty-p (string-trim parsed)))))
                         (cond
                          ((gptel-web-tools-bridge--result-error-p result)
                           (funcall bail (if blank
                                             "tool reported an error"
                                           parsed)))
                          ((null parsed) (funcall bail "could not parse result"))
                          (t (funcall succeed parsed)))))
                     (lambda (code message)
                       (funcall bail (format "%s: %s" code message))))
                    nil)
                (if (gptel-web-tools-bridge--maybe-connect server)
                    "server not connected, connecting now"
                  "server not connected"))
            (error (error-message-string err)))))
    (when reason (funcall bail reason))))


;;;; Native (eww) implementations

(defun gptel-web-tools-bridge--native-available-p ()
  "Return non-nil if gptel-agent's web tools can be used.
Loads `gptel-agent-tools' on first use."
  (or (featurep 'gptel-agent-tools)
      (require 'gptel-agent-tools nil t)))

(defun gptel-web-tools-bridge--native-unavailable (callback note)
  "Tell CALLBACK that no implementation is available, mentioning NOTE."
  (funcall callback
           (concat (gptel-web-tools-bridge--stamp "none" note)
                   "Error: no web provider is available and `gptel-agent-tools'\
 could not be loaded, so this tool cannot run.  Install gptel-agent, or set\
 `gptel-web-tools-bridge-provider' to a working provider.")))

(defun gptel-web-tools-bridge--native-search (callback query count note)
  "Search for QUERY with eww, calling CALLBACK with COUNT results.
NOTE explains, in the result's \"Source:\" line, why eww is being used."
  (if (gptel-web-tools-bridge--native-available-p)
      (gptel-agent--web-search-eww
       (lambda (text)
         (funcall callback
                  (concat (gptel-web-tools-bridge--stamp "eww" note) text)))
       query count)
    (gptel-web-tools-bridge--native-unavailable callback note)))

(defun gptel-web-tools-bridge--native-fetch (callback url note)
  "Fetch URL with eww and call CALLBACK with the text.
NOTE explains, in the result's \"Source:\" line, why eww is being used."
  (if (gptel-web-tools-bridge--native-available-p)
      (gptel-agent--read-url
       (lambda (text)
         (funcall callback
                  (concat (gptel-web-tools-bridge--stamp "eww" note) text)))
       url)
    (gptel-web-tools-bridge--native-unavailable callback note)))


;;;; Dispatchers, the functions behind the tools

(defun gptel-web-tools-bridge--search (callback query &optional count)
  "Search the web for QUERY and call CALLBACK with the results.
COUNT is the number of results wanted.  Routed to the provider named
by `gptel-web-tools-bridge-provider', or to eww."
  (let ((plist (gptel-web-tools-bridge--provider))
        (count (gptel-web-tools-bridge--result-count count)))
    (cond
     ((not (and (stringp query) (not (string-empty-p (string-trim query)))))
      (funcall callback "Error: `query' must be a non-empty string."))
     ((null plist)
      (gptel-web-tools-bridge--native-search
       callback query count (gptel-web-tools-bridge--native-note)))
     (t
      (gptel-web-tools-bridge--call
       "web_search" plist
       (plist-get plist :search-tool)
       (funcall (plist-get plist :search-args) query count)
       callback
       (lambda (note)
         (gptel-web-tools-bridge--native-search callback query count note)))))))

(defun gptel-web-tools-bridge--fetch (callback url &rest _)
  "Fetch URL and call CALLBACK with its contents as text.
Extra arguments are ignored, so that a model passing an extraction
prompt does not break the call."
  (let ((plist (gptel-web-tools-bridge--provider)))
    (cond
     ((not (and (stringp url) (not (string-empty-p (string-trim url)))))
      (funcall callback "Error: `url' must be a non-empty string."))
     ((null plist)
      (gptel-web-tools-bridge--native-fetch
       callback url (gptel-web-tools-bridge--native-note)))
     ((gptel-web-tools-bridge--fetch-natively-p url plist)
      (gptel-web-tools-bridge--native-fetch
       callback url "local or non-HTTP URL"))
     (t
      (gptel-web-tools-bridge--call
       "web_fetch" plist
       (plist-get plist :fetch-tool)
       (funcall (plist-get plist :fetch-args) url)
       callback
       (lambda (note)
         (gptel-web-tools-bridge--native-fetch callback url note)))))))


;;;; Tool descriptions and registration

(defconst gptel-web-tools-bridge--search-description
  "Search the web for current information.

Returns a ranked list of results as plain text: for each result its
title, its URL and an excerpt of the page.  The first line of the
returned text names the engine that served the call.  When an excerpt
is not enough, call `web_fetch' on the most promising URL to read the
whole page."
  "Provider-independent part of the `web_search' description.")

(defconst gptel-web-tools-bridge--fetch-description
  "Fetch a web page and return its contents as readable text rather
than HTML.

The first line of the returned text names the engine that served the
call."
  "Provider-independent part of the `web_fetch' description.")

(defconst gptel-web-tools-bridge--native-search-doc
  "No search provider is configured, so this tool is served by the
Emacs web browser (eww) and its default search engine.  It returns
about five results with short excerpts, and ignores `count'."
  "Paragraph appended to the `web_search' description in native mode.")

(defconst gptel-web-tools-bridge--native-fetch-doc
  "No fetch provider is configured, so this tool is served by the
Emacs web browser (eww).  YouTube URLs return the video description
and transcript instead of the page itself."
  "Paragraph appended to the `web_fetch' description in native mode.")

(defun gptel-web-tools-bridge--description (kind)
  "Return the description for KIND, either `search' or `fetch'.
The provider's own paragraph is appended to the fixed part."
  (let* ((plist (gptel-web-tools-bridge--provider))
         (base (if (eq kind 'search)
                   gptel-web-tools-bridge--search-description
                 gptel-web-tools-bridge--fetch-description))
         (extra (cond
                 ((null plist)
                  (if (eq kind 'search)
                      gptel-web-tools-bridge--native-search-doc
                    gptel-web-tools-bridge--native-fetch-doc))
                 ((eq kind 'search) (plist-get plist :search-doc))
                 (t (plist-get plist :fetch-doc)))))
    (if (and (stringp extra) (not (string-empty-p extra)))
        (concat base "\n\n" extra)
      base)))

(defun gptel-web-tools-bridge--args (kind)
  "Return a fresh argument specification for KIND.
The lists are built rather than quoted because `gptel-make-tool'
rewrites argument specifications in place."
  (if (eq kind 'search)
      (list (list :name "query" :type "string"
                  :description "The search query, in natural language.")
            (list :name "count" :type "integer" :optional t
                  :description "How many results to return.  Optional."))
    (list (list :name "url" :type "string"
                :description "The URL of the page to read."))))

(defun gptel-web-tools-bridge--owned-tools ()
  "Return the tools this bridge registers, as (CATEGORY NAME KIND FUNCTION)."
  (append
   (list (list "web" "web_search" 'search #'gptel-web-tools-bridge--search)
         (list "web" "web_fetch" 'fetch #'gptel-web-tools-bridge--fetch))
   (when gptel-web-tools-bridge-override-agent-tools
     (list (list "gptel-agent" "WebSearch" 'search
                 #'gptel-web-tools-bridge--search)
           (list "gptel-agent" "WebFetch" 'fetch
                 #'gptel-web-tools-bridge--fetch)))))

(defun gptel-web-tools-bridge--register (category name kind function)
  "Register NAME in CATEGORY as a KIND tool running FUNCTION."
  (gptel-make-tool
   :name name
   :category category
   :function function
   :description (gptel-web-tools-bridge--description kind)
   :args (gptel-web-tools-bridge--args kind)
   :async t
   :include t))

;;;###autoload
(defun gptel-web-tools-bridge-register-tools ()
  "Register the `web_search' and `web_fetch' gptel tools.
Safe to call again; it replaces any previous registration."
  (interactive)
  (gptel-web-tools-bridge--register
   "web" "web_search" 'search #'gptel-web-tools-bridge--search)
  (gptel-web-tools-bridge--register
   "web" "web_fetch" 'fetch #'gptel-web-tools-bridge--fetch))

;;;###autoload
(defun gptel-web-tools-bridge-install-agent-overrides ()
  "Point gptel-agent's `WebSearch' and `WebFetch' at this bridge.
Does nothing unless `gptel-agent-tools' can be loaded."
  (interactive)
  (if (not (gptel-web-tools-bridge--native-available-p))
      (message "gptel-web-tools-bridge: gptel-agent-tools is not available")
    (gptel-web-tools-bridge--register
     "gptel-agent" "WebSearch" 'search #'gptel-web-tools-bridge--search)
    (gptel-web-tools-bridge--register
     "gptel-agent" "WebFetch" 'fetch #'gptel-web-tools-bridge--fetch)))

(defun gptel-web-tools-bridge--refresh-descriptions ()
  "Update the registered tools' descriptions for the active provider.
The existing `gptel-tool' structs are modified in place, so buffers
that already hold them in `gptel-tools' see the new text."
  (pcase-dolist (`(,category ,name ,kind ,_function)
                 (gptel-web-tools-bridge--owned-tools))
    (when-let* ((tool (ignore-errors (gptel-get-tool (list category name)))))
      (when (gptel-tool-p tool)
        (setf (gptel-tool-description tool)
              (gptel-web-tools-bridge--description kind))))))


;;;; Server lifecycle

;;;###autoload
(defun gptel-web-tools-bridge-ensure-server (&optional callback)
  "Connect the active provider's MCP server unless it already is.

CALLBACK, if a function, is called with no arguments once the server
is up, or immediately when it already was.  Returns non-nil when the
server was already connected."
  (interactive)
  (let* ((plist (gptel-web-tools-bridge--provider))
         (server (plist-get plist :server))
         (interactive-p (called-interactively-p 'interactive)))
    (cond
     ((null plist)
      (when interactive-p
        (message "gptel-web-tools-bridge: no provider selected"))
      nil)
     ((gptel-web-tools-bridge--connection server)
      (when interactive-p
        (message "gptel-web-tools-bridge: %s is connected" server))
      (when (functionp callback) (funcall callback))
      t)
     ((null (assoc server mcp-hub-servers))
      (message "gptel-web-tools-bridge: no server named %S in `mcp-hub-servers'"
               server)
      nil)
     (t
      ;; A connection object left behind in any state other than
      ;; `connecting' would make `mcp--server-running-p' report the server
      ;; as running and `mcp-hub-start-all-server' skip it, so clear it out
      ;; first.  A handshake in progress is left alone.
      (let ((connection (gethash server mcp-server-connections)))
        (when (and connection (not (eq (mcp--status connection) 'connecting)))
          (ignore-errors (mcp-stop-server server))))
      (setq gptel-web-tools-bridge--last-connect (float-time))
      (condition-case err
          (progn
            (when interactive-p
              (message "gptel-web-tools-bridge: starting %s..." server))
            (mcp-hub-start-all-server callback (list server)))
        (error
         (message "gptel-web-tools-bridge: could not start %s: %s"
                  server (error-message-string err))))
      nil))))


;;;; Commands

(defconst gptel-web-tools-bridge--no-bridge-label "no bridge (eww)"
  "Completion candidate standing for a nil provider.")

(defun gptel-web-tools-bridge--read-provider ()
  "Read a provider key from the minibuffer.  Return nil for no bridge."
  (let* ((candidates
          (cons gptel-web-tools-bridge--no-bridge-label
                (mapcar (lambda (entry) (symbol-name (car entry)))
                        gptel-web-tools-bridge-providers)))
         (current (if gptel-web-tools-bridge-provider
                      (symbol-name gptel-web-tools-bridge-provider)
                    gptel-web-tools-bridge--no-bridge-label))
         (choice (completing-read
                  (format "Web tools provider (currently %s): " current)
                  candidates nil t nil nil current)))
    (unless (equal choice gptel-web-tools-bridge--no-bridge-label)
      (intern choice))))

;;;###autoload
(defun gptel-web-tools-bridge-set-provider (provider)
  "Serve `web_search' and `web_fetch' from PROVIDER.

PROVIDER is a key in `gptel-web-tools-bridge-providers', or nil to
use eww directly.  The tools' descriptions are updated in place and
the new provider's server is connected in the background.

This changes the current session only; customize
`gptel-web-tools-bridge-provider' to make it stick."
  (interactive (list (gptel-web-tools-bridge--read-provider)))
  (setq gptel-web-tools-bridge-provider provider)
  (gptel-web-tools-bridge--refresh-descriptions)
  (let ((plist (gptel-web-tools-bridge--provider)))
    (cond
     ((null provider)
      (message "gptel-web-tools-bridge: no bridge, web tools served by eww"))
     ((null plist)
      (message "gptel-web-tools-bridge: `%s' is not in `%s', falling back to eww"
               provider 'gptel-web-tools-bridge-providers))
     (t
      (gptel-web-tools-bridge-ensure-server)
      (message "gptel-web-tools-bridge: web tools served by %s"
               (plist-get plist :label))))))

;;;###autoload
(defun gptel-web-tools-bridge-test ()
  "Run one search and one fetch through the active provider.
Results, including their \"Source:\" lines, go to a display buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*gptel-web-tools-bridge-test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Provider: %s\nWaiting for web_search and web_fetch...\n"
                      (or gptel-web-tools-bridge-provider "none (eww)"))))
    (display-buffer buffer)
    (gptel-web-tools-bridge--search
     (lambda (text) (gptel-web-tools-bridge--test-insert buffer "web_search" text))
     "GNU Emacs release notes" 3)
    (gptel-web-tools-bridge--fetch
     (lambda (text) (gptel-web-tools-bridge--test-insert buffer "web_fetch" text))
     "https://example.com")))

(defun gptel-web-tools-bridge--test-insert (buffer label text)
  "Insert the first lines of TEXT into BUFFER under LABEL."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "\n=== %s ===\n%s\n"
                      label
                      (if (> (length text) 600)
                          (concat (substring text 0 600) "\n[...truncated]")
                        text))))))


;;;; Load-time setup

(gptel-web-tools-bridge-register-tools)

(with-eval-after-load 'gptel-agent-tools
  (when gptel-web-tools-bridge-override-agent-tools
    (gptel-web-tools-bridge-install-agent-overrides)))

(provide 'gptel-web-tools-bridge)
;;; gptel-web-tools-bridge.el ends here
