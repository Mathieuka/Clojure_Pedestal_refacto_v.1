(ns hello
  (:require [clojure.data.json :as json]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.content-negotiation :as conneg]))


;hash-map de mots interdit
(def unmentionable #{"Voldemort" "Sarbacane" "Osborne" "Flagada"})

;validation de la requete qui prend en paramètre le body de la resp.
(defn ok [body]
  {:status 200 :body body
   :headers {"Content-Type" "text/html"}}) ;on force l'intercepteur de servlet a de definir le type de contenu "text/html"


;------------------------------------------------echo interceptor-------------------------------------------------------

;Definition de l'interceptor echo
(def echo
  {:name ::echo
   :enter #(assoc % :response (ok (:request %)))})


;---------interceptor fournis par pedestal qui permet de gerer la specification de négociation de contenu HTTP----------

;definition des types supportés
(def supported-types ["text/html" "application/edn" "application/json" "text/plain"])

;on passe les types supportés a la fonctions de negotiation.
(def content-neg-intc (conneg/negotiate-content supported-types))



;-------------------------Gestion de la validité du nom donné par l'interceptor respond-hello---------------------------

;fonction simple de verification que le params fournis par le client n'est pas vide
;plus facile a tester du fait qui soit extrait et autonome par rapport au gestionnaire.
(defn greeting-for [nm]
  (cond                                                     ;condition macro
    (unmentionable nm) nil                                  ;si le params fait partie des mots interdit alors il vaut nil
    (empty? nm)        "Hello, world!\n"                    ;sinon si le params est vide alors on affiche "Hello, world!".
    :else              (str "Hello, " nm "\n")))            ;sinon on affiche "Hello " params

;gestionnaire qui coordonne le reste "Qui est aussi un interceptor mais wrappé par Pedestal".
(defn respond-hello [request]
  (let [nm (get-in request [:query-params :name])
        resp (greeting-for nm)]
    (if resp                                                ;si il y a une reponse
      (ok resp)                                             ;on appel la fonction (ok)
      )))                                                   ;sinon on retourne (not-found)



;--------------------------Gestion de l'en tète dynamique avec l'inteceptor coerce-body---------------------------------

(defn transform-content
  [body content-type]
  (case content-type
    "text/html"        body
    "text/plain"       body
    "application/edn"  (pr-str body)
    "application/json" (json/write-str body)))




(defn coerce-to
  [response content-type]
  (-> response
      (update :body transform-content content-type)   ;(transform-content) prend le content-type en argument et applique la fonction
      (assoc-in [:headers "Content-type"] content-type)))   ; on associe le nouveau content type au context

(defn accepted-type
  [context]
  (get-in context [:request :accept :field] "text/plain"))


;fonction INTERCEPTOR qui regarde a l'interieur du context, si il y a déja un en tête content type
;alors il ne fait rien sinon il modifie le contexte en mettant a jour la réponse grâce
;à la fonction (accepted-type)
(def coerce-body
  {:name ::coerce-body
   :leave
   (fn [context]
     (cond-> context
             (nil? (get-in context [:response :headers "Content-Type"])) ;si Content-type == nil alors on update-in :response sinon on retourne simplement le context
             (update-in [:response] coerce-to (accepted-type context))))}) ;(accepted-type) == recuperation du Content-Type donné par le client
                                                                           ;(coerce-to) est la fonction appliquer a la réponse, il prend donc la réponse en parametre
                                                                           ;et le retour de (accepted type)




;------------------------------------------definition des routes--------------------------------------------------------

(def routes
  (route/expand-routes
    #{["/greet" :get [coerce-body content-neg-intc respond-hello] :route-name :greet]     ;cet route a un vecteur d'intercepteur a invoquer
      ["/echo" :get echo]}))

;--------------------------------------------connection au serveur------------------------------------------------------

(defn create-server []
  (http/create-server
    {::http/routes routes
     ::http/type :jetty
     ::http/port 8090}))


;lancement du serveur
(defn start []
  (http/start (create-server)))












;Ancienne définition de echo comme interceptor
#_(def echo
    {:name ::echo                                             ;on donne un nom a l'interceptor pour nous aider au deboggage
     :enter (fn [context]                                     ;on créer une fonction d'entrée qui prend un context en paramètre
              (let [request (:request context)                ;on extrait la map de requete de la map de context
                    response (ok request)]                    ;on prend la "request" que l'on a extrait précedemment et on en fait une réponse.
                (assoc context :response response)))})        ;enfin on attache la réponse à notre context


; Ancien interceptor qui permet de gerer les differents changement de content type de façon dynamique
#_(def coerce-body
    {:name ::coerce-body
     :leave
           (fn [context]
             (let [accepted   (get-in context [:request :accept :field] "text/plain")
                   response   (get context :response)
                   body       (get response :body)
                   coerce-body (case accepted
                                 "text/html"        body
                                 "text/plain"       body
                                 "application/edn"  (pr-str body)
                                 "application/json" (json/write-str body))
                   update-response (assoc response
                                     :headers {"Content-Type" accepted}
                                     :body    coerce-body)]
               (assoc context :response update-response)))})
