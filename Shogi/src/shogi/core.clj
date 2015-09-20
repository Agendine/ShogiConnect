;; *****************************************************************************************
;; *****************************************************************************************
;; This is the draft file for the logic of a Shogi game, intended as a phone app allowing
;; asynchronous play over the internet, using push notification.  Early draft does not
;; include single-player-versus-ai, so all moves are input by a player.  The goal is
;; for the code to be as streamlined and un-redundant as possible, and to allow simple
;; verification, feature implementation/extensibility, error-checking, and for the code to
;; be easily understandable.  This is achieved by following a thoroughly-planned and minimalistic
;; architecture, and through documentation.
;;
;; Though the program is being initially created for Android due to my own familiarity,
;;      it is designed to be as easy to implement on other devices as possible,
;;      largely by relying only on services to which every phone OS has a parallel, and by
;;      using the server to handle as much as possible. Lein Droid is being used for
;;      the actual Android app.
;; *****************************************************************************************
;; *****************************************************************************************
;;
;; **************************************************
;; The phone side of things contains:
;; **************************************************
;;      The game logic,
;;      UI and options,
;;      SQLite database of current games (stored as JSON for easy parse and transmission)
;;      parsing and creation protocols for the JSON game objects,
;;      and communication with the server.
;;      **possibly later:**
;;           Player's game history, so they can review wins and losses
;;           AI (s?) to play against
;;
;; **************************************************
;; The server side handles:
;; **************************************************
;;      User accounts (VERY simple.  NO PRIVATE INFORMATION IS STORED.  Just:
;;                     a username,
;;                     a way to send push notifications,
;;                     a running game list (**possible game history?**) so that games
;;                              can be recovered after switching devices),
;;                     Ranked win/loss record and Rating,
;;                     a friend list,
;;                     and **possibly, later** an Avatar to display.
;;      matchmaking (simple ranked or unranked player pools.  A player can simply request to
;;                   be listed as available for a match, or browse the list of available
;;                   Players to challenge)
;;      notifications to the other player (via Push notificaitons through Google/Apple services),
;;      user verification (via Google/Apple account authentication, since those should be
;;                         available for any user)
;;      Game history of each running game (used to verify that no illegal
;;                    JSON-tampering gets sent as a move, to prevent cheating).
;;
;; **************************************************
;; * Organization:
;; **************************************************
;;
;; A game state is defined purely by the board, so that it can easily be stored and transmitted
;;         as a simple JSON object.
;;
;; Board:  A Board is:
;;         A 9x9 matric of "Spaces", each of which can either be "Empty" or contain a "Piece".
;;               The board is laid out as follows:
;;
;;                 Row
;;                  9
;;                  8                Note that the coordinates are 1-based (for easy
;;                  .                 transcription) and labelled XY-style, increasing
;;                  .                 bottom-to-top and left-to-right.
;;                  .
;;                  1  Col 1 2 3 ... 9
;;
;;
;;         2 "Players",
;;         2 "Hands", one for each player (a set of captured
;;         Pieces, playable as per the rules).
;;                Thus, "Locations" which can contain Pieces
;;                includes both an (x,y) grid location or "Player 1's Hand" or
;;                "Player 2's Hand".  A Piece, itself, thus doesn't have to store
;;                it's own location.
;;         A Current Turn variable, for who's turn it is.
;;
;; Piece: a Piece has:
;;        an Owner (Player 1 or Player 2),
;;        a Type (King, Rook, Bishop, Gold General, Silver General, Knight, Lance, Pawn,
;;                Promoted Rook, Promoted Bishop, Promoted Silver General, Promoted Knight,
;;                Promoted Lance, or Promoted Pawn),
;;
;; Type: a Type has:
;;         List of Movement capabilities (functions and parameters),
;;        "Is Promotable?" value (boolean),
;;        "Promotion" value (a Type), either what it promotes to (if Promotable),
;;                  what it promotes from (if already promoted, as these can revert if captured),
;;                  or nil if King or Gold General.
;;         Name (a simple keyword),
;;         Graphic (a way to call the appropriate image file)
;;
;; Player: a Player has:
;;         Direction (multiplier, +1 or -1), to be applied to their piece's movement directions.
;;                    This is also used to represent who goes first, and to link to that player's
;;                    "Hand".
;;         Send Move Function, to be called when a move is confirmed to submit to the other player.
;;                    This allows for moves to be "sent" to a local game (on same device), or
;;                    to the Server for remote player.
;;         **In Future**:
;;                Move Function, to allow for AI players to be represented by the same
;;                basic implementation as Human players, with a single "over-ridden" function.
;;
;;
;; *****************************************************************************************
;; Author: Agendine (Matthew Kroen), started 26 August 2015.
;; *****************************************************************************************






;; *****************************************************************************************
;; Project Setup:
;; *****************************************************************************************

(ns shogi.core
  (:gen-class)
  (:require [cheshire.core :refer :all])
  (:use clojure.test))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))





;; *****************************************************************************************
;; Generic Utility Functions:
;; *****************************************************************************************

(defn filter-keys-by-val
  "Returns all keys in map for which (pred value) returns true."
  [pred m]
  (when m
    (for [[key val] m :when (pred val)] key)))

(defn not-empty [val]
  "Simple predicate for comparing a value to empty"
  (false? (= val "empty")))



;; Utility Functions For Board Setup
;; ---------------------------------------------

(declare board)



;; *****************************************************************************************
;; Movement:
;; *****************************************************************************************

;; Utility functions for movement
;; ---------------------------------------------

(defn is-on-board
  "Utility function to determine if a space is on the board"
  [x-coord y-coord]
  (and (and (> x-coord 0) (<= x-coord 9))
       (and (> y-coord 0) (<= y-coord 9))))

;; Core Movement Logic:
;; ---------------------------------------------

(defn move-direction
  "Quick test implementation for generic movement in a direction, called recursively.
    Actually just tests if the current space is a legal move, and if so whether to
    then test the next space in that direction, or else return a vector of the legal
    moves found. direction-x/y is +1 or -1 (- is up/left, + is down/right) and
    origin-x/y refers to the space being tested.  The remaining parameter allows
    for having either one-space or full-board movement in that direction.
    The accumulator parameter should be [] on an initial call, and will be returned
    as [[x y] [x2 y2]] style list of coordinate-pairs of legal moves.
    The player parameter indicates the moving piece's owner.
    Returns a vector of available moves in the direction."
  [[direction-x direction-y origin-x origin-y remaining player]]
  ;; If the current space is on-board, continue, else end checking in this direction.
  (if (is-on-board origin-x origin-y)
    ;; If the piece has enough movement for the distance, continue, else end
    (if (< 0 remaining)
      ;; If the current space is unoccupied, then proceed on this direction, else
      ;;    check if it's player's or opponent's and handle:
      (if (= (get-in board [origin-x origin-y]) nil)
        (let [further-moves  (move-direction
                               [direction-x direction-y
                                (+ direction-x origin-x) (+ direction-y origin-y)
                                (- remaining 1) player])]
             (into further-moves [[origin-x origin-y]]))
        ;; If the space is occupied by Player's Piece, stop checking this direction, else
        ;; handle as either capturable Piece or Check/Checkmate
        (if (= (get-in board [origin-x origin-y :owner]) player)
          []
          ;;        (TODO: INDICATE CAPTURABLE SEPERATELY)
          [[origin-x origin-y]]))
      [])
    []))


(defn move-horizontal
  "Function for handling the fact that this piece can move left and right,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (let [available-moves (move-direction [-1 0 origin-x origin-y spaces player])]
    (into [] (distinct (into (move-direction [1 0 origin-x origin-y spaces player])
                             available-moves)))))

(defn move-diagonal
  "Function for handling the fact that this piece can move in 4 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (let [available-moves-left (into (move-direction [-1 -1
                                                    (- origin-x 1) (- origin-y 1)
                                                    spaces player])
                                   (move-direction [-1 1
                                                    (- origin-x 1) (+ origin-y 1)
                                                    spaces player]))
        available-moves-right (into (move-direction [1 -1
                                                     (+ origin-x 1) (- origin-y 1)
                                                     spaces player])
                                    (move-direction [1 1
                                                     (+ origin-x 1) (+ origin-y 1)
                                                     spaces player]))]
    (into available-moves-left available-moves-right)))

(defn move-diagonal-forward
  "Function for handling the fact that this piece can move in 2 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (let [available-moves-left (move-direction [-1 player
                                              (- origin-x 1) (+ player origin-y)
                                              spaces player])
        available-moves-right (move-direction [1 player
                                              (+ origin-x 1) (+ player origin-y)
                                               spaces player])]
    (into available-moves-left available-moves-right)))


(defn move-forward
  "Function for handling the fact that this piece can move forward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (move-direction [0 player origin-x (+ player origin-y) spaces player]))

(defn move-backward
  "Function for handling the fact that this piece can move backward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (move-direction [0 (* player -1) origin-x (- origin-y player) spaces player]))

(defn move-jump
  "Function for handling Knights' ability to move by jumping to spaces.
   NOTE: the spaces parameter is kept, but not used, to ease generic movement-function calling.
   Returns a list of available jumps, or an empty vector if none are available."
  [spaces origin-x origin-y player]
  (into (move-direction [1 1 (+ origin-x 1) (+ origin-y (* 2 player)) 1 player])
        (move-direction [1 1 (- origin-x 1) (+ origin-y (* 2 player)) 1 player])))





;; *****************************************************************************************
;; Board Setup:
;; *****************************************************************************************


;; Type Definition and Initialization:
;; ---------------------------------------------

;; (declare king-type)
;; (declare rook-type)
;; (declare promoted-rook-type)
;; (declare bishop-type)
;; (declare promoted-bishop-type)
;; (declare gold-general-type)
;; (declare silver-general-type)
;; (declare promoted-silver-general-type)
;; (declare knight-type)
;; (declare promoted-knight-type)
;; (declare lance-type)
;; (declare promoted-lance-type)
;; (declare pawn-type)
;; (declare promoted-pawn-type)

(def king-type {:moves [['move-horizontal 1]
                        ['move-forward 1]
                        ['move-backward 1]
                        ['move-diagonal 1]]
                :is-promotable false
                :promotion 'king-type
                :name "King"})

(def rook-type {:moves [['move-horizontal 9]
                        ['move-forward 9]
                        ['move-backward 9]]
                :is-promotable true
                :promotion 'promoted-rook-type
                :name "Rook"})

(def promoted-rook-type {:moves [['move-horizontal 1]
                                 ['move-forward 1]
                                 ['move-backward 1]
                                 ['move-diagonal 9]]
                :is-promotable false
                :promotion 'rook-type
                :name "PromotedRook"})

(def bishop-type {:moves [['move-diagonal 9]]
                :is-promotable true
                :promotion 'promoted-bishop-type
                :name "Bishop"})

(def promoted-bishop-type {:moves [['move-horizontal 1]
                                   ['move-forward 1]
                                   ['move-backward 1]
                                   ['move-diagonal 9]]
                :is-promotable false
                :promotion 'bishop-type
                :name "PromotedBishop"})

(def gold-general-type {:moves [['move-horizontal 1]
                                ['move-forward 1]
                                ['move-backward 1]
                                ['move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'gold-general-type
                :name "GoldGeneral"})

(def silver-general-type {:moves [['move-forward 1]
                                  ['move-diagonal 1]]
                :is-promotable true
                :promotion 'promoted-silver-general-type
                :name "SilverGeneral"})

(def promoted-silver-general-type {:moves [['move-horizontal 1]
                                           ['move-forward 1]
                                           ['move-backward 1]
                                           ['move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'silver-general-type
                :name "PromotedSilverGeneral"})

(def knight-type {:moves [['move-jump 1]]
                :is-promotable true
                :promotion 'promoted-knight-type
                :name "Knight"})

(def promoted-knight-type {:moves [['move-horizontal 1]
                                   ['move-forward 1]
                                   ['move-backward 1]
                                   ['move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'knight-type
                :name "PromotedKnight"})

(def lance-type {:moves [['move-forward 9]]
                :is-promotable true
                :promotion 'promoted-lance-type
                :name "Lance"})

(def promoted-lance-type {:moves [['move-horizontal 1]
                                  ['move-forward 1]
                                  ['move-backward 1]
                                  ['move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'lance-type
                :name "PromotedLance"})

(def pawn-type {:moves [['move-forward 1]]
                :is-promotable true
                :promotion 'promoted-pawn-type
                :name "Pawn"})

(def promoted-pawn-type {:moves [['move-horizontal 1]
                                 ['move-forward 1]
                                 ['move-backward 1]
                                 ['move-diagonal-forward 1]]
                :is-promotable false
                :promotion 'pawn-type
                :name "PromotedPawn"})



;; Type Definition and Initialization:
;; ---------------------------------------------

(defn create-pieces
  "Function to instantiate Piece Types.  Creates the specified number of Pieces, of
   the specified Type, distributed evenly between the two players' ownership.
   NOTE: a Piece has: :owner and :type"
  [[piece-type piece-amount]]
  (loop [index 1]
    (do
      (eval `(def ~(symbol (str (piece-type :name) index)) {:owner 1 :type '~piece-type}))
      (eval `(def ~(symbol (str (piece-type :name) (+ 1 index))) {:owner -1 :type '~piece-type}))
      (if (< index piece-amount)
        (recur (+ 2 index))))))

(defn create-mass-pieces
  "Function to call create-pieces on a list of Type-Amount pairs, suitable for
   initializing the Board at start of game."
  [list-of-pairs]
  (map create-pieces list-of-pairs))

(defn initialize-pieces
  "This function creates all starting pieces at once, but they still have to be placed
   on the board."
  []
  (create-mass-pieces
   [[king-type 2]
    [rook-type 2]
    [bishop-type 2]
    [gold-general-type 4]
    [silver-general-type 4]
    [knight-type 4]
    [lance-type 4]
    [pawn-type 18]]))

(defn pretty-print
  "DRAFT 2: Development method for pretty-printing the board.
  This one is adjusted for the (x y) coordinate lookup system."
  []
  (map (fn [arg1]
         (println (apply str
                         (map #(if (nil? (get-in board [% arg1 :type :name]))
                                 (str "     | ")
                                 (str (get-in board [% arg1 :type :name]) " | "))
                              (range 1 10)))))
       (reverse (range 1 10))))


(defn pretty-test
  "Captures a pretty print's output for uses besides printing it."
  []
  (map (fn [arg1]
         (with-out-str (println (apply str
                                       (map #(if (nil? (get-in board [% arg1 :type :name]))
                                               (str "     | ")
                                               (str (get-in board [% arg1 :type :name]) " | "))
                                            (range 1 10))))))
       (reverse (range 1 10))))


;; Board Definition and Initialization:
;; ---------------------------------------------

(defn setup-board
  "Function to set up the board for the initial gamestate.  Initializes all starting pieces,
   and sets up the board with each piece in its proper starting place.
   Also defines the board object and the game which containts that board.
   Note that these are all, essentially, global variables, but also all are
   ultimately contained, hierarchically, under the game map."
  []
  (do
    (initialize-pieces)

    (def col1 (sorted-map 1 Lance1 2 nil 3 Pawn1 4 nil 5 nil 6 nil 7 Pawn2
                          8 nil  9 Lance4))
    (def col2 (sorted-map 1 Knight1 2 Rook1 3 Pawn3 4 nil 5 nil 6 nil 7 Pawn4
                          8 Bishop2 9 Knight4))
    (def col3 (sorted-map 1 SilverGeneral1 2 nil 3 Pawn5 4 nil 5 nil 6 nil 7 Pawn6
                          8 nil  9 SilverGeneral4))
    (def col4 (sorted-map 1 GoldGeneral1 2 nil 3 Pawn7 4 nil 5 nil 6 nil 7 Pawn8
                          8 nil 9 GoldGeneral2))
    (def col5 (sorted-map 1 King1 2 nil 3 Pawn9 4 nil 5 nil 6 nil 7 Pawn10 8 nil
                          9 King2))
    (def col6 (sorted-map 1 GoldGeneral3 2 nil 3 Pawn11 4 nil 5 nil 6 nil 7 Pawn12
                          8 nil 9 GoldGeneral4))
    (def col7 (sorted-map 1 SilverGeneral3 2 nil 3 Pawn13 4 nil 5 nil 6 nil 7 Pawn14
                          8 nil 9 SilverGeneral4))
    (def col8 (sorted-map 1 Knight3 2 Bishop1 3 Pawn15 4 nil 5 nil 6 nil 7 Pawn16
                          8 Rook2 9 Knight4))
    (def col9 (sorted-map 1 Lance3 2 nil 3 Pawn17 4 nil 5 nil 6 nil 7 Pawn18
                          8 nil 9 Lance4))
    (def board (sorted-map 1 col1 2 col2 3 col3 4 col4 5 col5 6 col6 7 col7
                           8 col8 9 col9))

    (def hand1 [])
    (def hand2 [])
    (def player1 {:player -1 :hand hand1 :in-check? false})
    (def player2 {:player 1 :hand hand2 :in-check? false})
    (def turn player1)

    (def game (hash-map :board board :turn turn -1 player1 1 player2))))


(defn update-game
  "IMPORTANT: This function can be called at any time to update the game object to store the
   current state of the board, player-hands, turn counter, etc.
  THIS MUST BE CALLED AFTER EVERY TURN, before storing the game to JSON!"
  []
  (def game (hash-map :board board :turn turn -1 player1 1 player2)))


;; Player and Board Resets (for development):
;; ---------------------------------------------

(defn reset-hands
  []
  (def hand1 [])
  (def hand2 [])
  (def player1 {:player -1 :hand hand1 :in-check? false})
  (def player2 {:player 1 :hand hand2 :in-check? false})
  (def turn player1)
  (update-game))



 ;; *****************************************************************************************
;; Movement:
;; *****************************************************************************************

(defn get-other-player
  "Quick utility function to return a reference to the actual Player map opposite the parameter
   Player. Useful for quickly obtaining the other player's Hand."
  [player]
  (if (= player (player1 :player))
    player2
    player1))

(defn query-all-moves
  "DRAFT: function to aggregate all open moves from all movement methods of
  an unknown piece, queried by board position.  NOTE: THESE RESULTS ARE RAW! Does NOT exclude
  moves for moving into check or other secondary rules violations."
  [piece-x piece-y]
  (distinct (reduce into (map
                          (fn [[move-function num-spaces]]
                            ((eval move-function) num-spaces
                                           piece-x piece-y
                                           (get-in board [piece-x piece-y :owner])))
                          (get-in board [piece-x piece-y :type :moves])))))

(defn query-all-moves-for-player
  "Function aggregates all possible spaces to which the parameter player's pieces could move
  semi-legally.
  NOTE THAT THIS IS RAW DATA!  Secondary-rules violations, such as moving into check, are NOT checked
  at this stage.  All moves in the output vector are distinct, but are not matched to any particular
  moving piece."
  [player]
  (remove #(= [] %) (distinct
                     (reduce into
                             (map (fn [y-coord]
                                    (reduce into (map
                                                  (fn [x-coord] (if (= (get-in board [x-coord
                                                                                      y-coord
                                                                                      :owner])
                                                                       player)
                                                                  (query-all-moves x-coord y-coord)
                                                                  []))
                                                  (range 1 10))))
                                  (range 1 10))))))


(defn is-space-reachable-by-player?
  "Returns boolean result for whether any of the specified player's pieces could semi-legally
   reach the specified board space."
  [x-coord y-coord player]
  (some #(= [x-coord y-coord] %) (query-all-moves-for-player player)))

(defn is-space-reachable-by-piece?
  "Returns boolean result for whether a specified piece could (semi-)legally
   reach the specified board space."
  [from-x from-y to-x to-y]
  (some #(= [to-x to-y] %) (query-all-moves from-x from-y)))

(defn capture-piece
       "DRAFT: Conducts the actual capture of a piece, simply adding it to the attacker's hand
               and exchanging ownership of the piece.  Only call this function when capture
               has already been verified for legality.  This function assumes that the proper
               player is performing the capture.
   TODO LATER: De-promote as well.
               Test more thoroughly, particularly that both game and board update properly."
       [captured-x captured-y]
       (do
         (if (= (get-in board [captured-x captured-y :owner]) (player2 :player))
           (def player1
             (assoc player1 :hand (conj (player1 :hand)
                                        (assoc (get-in board [captured-x captured-y])
                                               :owner (player1 :player)))))
           (def player2
             (assoc player2 :hand (conj (player2 :hand)
                                        (assoc (get-in board [captured-x captured-y])
                                               :owner (player2 :player))))))
         (update-game)))


(defn move-piece
  "Changes the game state to reflect a piece's movement.  Will deny outright-illegal moves,
    will invoke capture if moving to a space occupied by an opposing piece, will update the global
    Game state before returning.
  TODO LATER:  implement check/checkmate tracking,
               implement promotion,
               implement turn switching."
  [from-x from-y to-x to-y]
  (do
    (if (is-space-reachable-by-piece? from-x from-y to-x to-y)
      (let [moving-piece (get-in board [from-x from-y])]
        (def board
          (assoc-in (assoc-in board [from-x from-y] nil)
                    [to-x to-y] moving-piece))
        (if (not (nil? (get-in board [to-x to-y])))
          (capture-piece to-x to-y)))
      (println "Illegal Move.  Space Not Reachable."))
    (update-game)))


(defn locate-king
  "DRAFT: utility function which outputs an [x y] coordinate vector containing the location
  of the specified player's king."
  [player]
  (first
   (for [x (range 1 10) y (range 1 10)
         :let [coords [x y]]
         :when (and (= (get-in board [x y :type :name]) "King")
                    (= (get-in board [x y :owner]) player))]
     coords)))

;; (defn locate-king
;;   "DRAFT: utility function which outputs an [x y] coordinate vector containing the location
;;   of the specified player's king."
;;   []player]
;;   (loop [y-coord 1]
;;     (loop [x-coord 1]
;;       (if (and (= (get-in board [x-coord y-coord :type :name]) "King")
;;                (= (get-in board [x-coord y-coord :owner]) player))
;;         (println [x-coord y-coord])
;;         (if (< x-coord 10)
;;           (recur (inc x-coord)))))
;;     (if (< y-coord 10)
;;       (recur (inc y-coord))))


(defn is-in-check?
  "TESTING: DRAFT: simple boolean result for whether any of the opposing player's pieces have
   the parameter player's king as an available move.
  NOTE: DOES NOT conduct secondary checks to determine if those moves
                 can be completed legally (yet).  For example, it will not notice that
                 capture could only be attempted by putting oneself in check."
   [player]
  (let [[king-x king-y] (locate-king player)
        opposing-player ((get-other-player player) :player)]
    (not (nil? (is-space-reachable-by-player? king-x king-y opposing-player)))))

(defn is-in-checkmate?
  "TESTING: DRAFT: Simple boolean result for whether parameter player is currently in checkmate.
  TODO: Expand the search for moves which could displace check beyond just the king itself.
        Check for move repetition for stalemate."
  [player]
  (let [[king-x king-y] (locate-king player)]
    (if (and (is-in-check? player) (empty? (query-all-moves king-x king-y))) (true))))


(defn drop-piece
  "DRAFT: Removes a previously captured piece from the player's hand, and places it on
          an empty space on the boart.
  TODO:   Solidify checks for rules compliance."
  [piece to-x to-y player]
  (if (nil? (get-in board [to-x to-y]))
    (do
      (def board
        (assoc-in board [to-x to-y] piece))
      (def player
        (remove #{piece} (get-in player [:hand])))
      (update-game))
    (println "Illegal Drop.  Destination not empty.")))

;; TO DO:
;;        Query-all-in-hand
;;              ~for purposes of JSONification
;;        Promotions
;;             -isInPromotionZone
;;             -handlePromotion
;;             --ensure pieces aren't dropped into promotion or checkmate
;;        Turn advancement and enforcement.
;;             --checkmate compliances
;;        More thorough testing.






;; *****************************************************************************************
;; JSON and SQLite:
;; *****************************************************************************************

(defn board-to-json
  "Uses Cheshire's encode [== generate-string] to encode the state of the board to JSON"
  [] (encode board))

(defn game-to-json
  "Uses Cheshire's encode [== generate-string] to encode the full game state to JSON"
  [] (encode board))


;; TODO
;;   --------IMPORTANT------------
;;   -Set up Parse from JSON to game state.
;;   -----------------------------
;;  -



;; *****************************************************************************************
;; Unit testing framework:
;; *****************************************************************************************

(defn board-fixture [function-in]
  setup-board
  (function-in))

(deftest test-setup
  (is (= (str (get-in board [3 1])
          "{:type {:is-promotable true, :moves [[move-forward 1] [move-diagonal 1]], :name \"SilverGeneral\", :promotion promoted-silver-general-type}, :owner 1}")))
  (is (= (str King1) "{:type {:is-promotable false, :moves [[move-horizontal 1] [move-forward 1] [move-backward 1] [move-diagonal 1]], :name \"King\", :promotion king-type}, :owner 1}")))


(deftest test-move-queries
  (is (= (move-direction [1 1 3 4 9 -1])
         [[5 6] [4 5] [3 4]]))
  (is (= (move-direction [1 1 3 4 9 1])
         [[6 7] [5 6] [4 5] [3 4]]))
  (is (= (move-direction [1 1 7 7 9 1]) [[7 7]]))
  (is (= (move-direction [1 1 1 1 9 1]) []))

  (is (= (move-horizontal 2 5 5 1) [[6 5] [5 5] [4 5]]))
  (is (= (move-horizontal 2 5 5 -1) [[6 5] [5 5] [4 5]]))
  (is (= (move-horizontal 2 3 3 -1) [[3 3]]))
  (is (= (move-horizontal 2 3 3 1) []))

  (is (= (move-diagonal 9 5 5 1) [[4 4] [3 7] [4 6] [6 4] [7 7] [6 6]]))
  (is (= (move-diagonal 9 5 5 -1) [[3 3] [4 4] [4 6] [7 3] [6 4] [6 6]]))
  (is (= (move-diagonal 5 3 3 -1) [[2 2] [1 5] [2 4] [5 1] [4 2] [6 6] [5 5] [4 4]]))
  (is (= (move-diagonal 9 3 3 1) [[1 5] [2 4] [4 2] [7 7] [6 6] [5 5] [4 4]]))

  (is (= (move-diagonal-forward 9 5 5 1) [[3 7] [4 6] [7 7] [6 6]]))
  (is (= (move-diagonal-forward 9 5 5 -1) [[3 3] [4 4] [7 3] [6 4]]))
  (is (= (move-diagonal-forward 9 3 3 -1) [[2 2] [5 1] [4 2]]))
  (is (= (move-diagonal-forward 9 3 3 1) [[1 5] [2 4] [7 7] [6 6] [5 5] [4 4]]))

  (is (= (move-forward 9 5 5 1) [[5 7] [5 6]]))
  (is (= (move-forward 9 5 5 -1) [[5 3] [5 4]]))
  (is (= (move-forward 9 3 3 -1) [[3 1] [3 2]]))
  (is (= (move-forward 9 3 3 1) [[3 7] [3 6] [3 5] [3 4]]))

  (is (= (move-backward 9 5 5 1) [[5 4]]))
  (is (= (move-backward 9 5 5 -1) [[5 6]]))
  (is (= (move-backward 9 3 3 -1) [[3 6] [3 5] [3 4]]))
  (is (= (move-backward 9 3 3 1) [[3 2]]))

  (is (= (move-jump 9 5 5 1) [[6 7] [4 7]]))
  (is (= (move-jump 9 5 5 -1) [[6 3] [4 3]]))
  (is (= (move-jump 9 3 3 -1) [[4 1] [2 1]]))
  (is (= (move-jump 9 3 3 1) [[4 5] [2 5]]))

  (is (= (query-all-moves 8 1) ()))

  (is (=
       (with-out-str (println (doall (query-all-moves-for-player 1))))
       "([1 4] [2 4] [3 4] [4 4] [5 4] [6 4] [7 4] [8 4] [9 4] [9 2] [6 2] [7 2] [5 2] [4 2] [3 2] [1 2])\n"))

  (is (=
       (with-out-str (println (doall (query-all-moves-for-player -1))))
       "([9 6] [8 6] [7 6] [6 6] [5 6] [4 6] [3 6] [2 6] [1 6] [9 8] [6 8] [7 8] [5 8] [4 8] [3 8] [1 8])\n")))


(deftest test-all
  (use-fixtures :once board-fixture)
  (test-setup)
  (test-move-queries))
;;   (test-movement)
;;   (test-capture)
;;   (test-dropping)
;;   (test-check-detection))


