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
  (:require [clojure.java.jdbc :refer :all :as jdbc])
  (:use clojure.test))
;;  (:require [sqlingvo.core :refer :all])
;;  (:require [sqlingvo.db :as db]))
;;  (:use (lobos connectivity core schema))
;;  (:use korma.db)
;;  (:use korma.core))

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

;; (declare board)



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
  [[board direction-x direction-y origin-x origin-y remaining player]]
  ;; If the current space is on-board, continue, else end checking in this direction.
  (if (is-on-board origin-x origin-y)
    ;; If the piece has enough movement for the distance, continue, else end
    (if (< 0 remaining)
      ;; If the current space is unoccupied, then proceed on this direction, else
      ;;    check if it's player's or opponent's and handle:
      (if (= (get-in board [origin-x origin-y]) nil)
        (let [further-moves  (move-direction
                               [board direction-x direction-y
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
  [board spaces origin-x origin-y player]
  (let [available-moves (move-direction [board -1 0 origin-x origin-y spaces player])]
    (into [] (distinct (into (board move-direction [1 0 origin-x origin-y spaces player])
                             available-moves)))))

(defn move-diagonal
  "Function for handling the fact that this piece can move in 4 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [board spaces origin-x origin-y player]
  (let [available-moves-left (into (move-direction [board -1 -1
                                                    (- origin-x 1) (- origin-y 1)
                                                    spaces player])
                                   (move-direction [board -1 1
                                                    (- origin-x 1) (+ origin-y 1)
                                                    spaces player]))
        available-moves-right (into (move-direction [board 1 -1
                                                     (+ origin-x 1) (- origin-y 1)
                                                     spaces player])
                                    (move-direction [board 1 1
                                                     (+ origin-x 1) (+ origin-y 1)
                                                     spaces player]))]
    (into available-moves-left available-moves-right)))

(defn move-diagonal-forward
  "Function for handling the fact that this piece can move in 2 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [board spaces origin-x origin-y player]
  (let [available-moves-left (move-direction [board -1 player
                                              (- origin-x 1) (+ player origin-y)
                                              spaces player])
        available-moves-right (move-direction [board 1 player
                                              (+ origin-x 1) (+ player origin-y)
                                               spaces player])]
    (into available-moves-left available-moves-right)))


(defn move-forward
  "Function for handling the fact that this piece can move forward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [board spaces origin-x origin-y player]
  (move-direction [board 0 player origin-x (+ player origin-y) spaces player]))

(defn move-backward
  "Function for handling the fact that this piece can move backward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (move-direction [board 0 (* player -1) origin-x (- origin-y player) spaces player]))

(defn move-jump
  "Function for handling Knights' ability to move by jumping to spaces.
   NOTE: the spaces parameter is kept, but not used, to ease generic movement-function calling.
   Returns a list of available jumps, or an empty vector if none are available."
  [board spaces origin-x origin-y player]
  (into (move-direction [board 1 1 (+ origin-x 1) (+ origin-y (* 2 player)) 1 player])
        (move-direction [board 1 1 (- origin-x 1) (+ origin-y (* 2 player)) 1 player])))





;; *****************************************************************************************
;; Board Setup:
;; *****************************************************************************************


;; Type Definition and Initialization:
;; ---------------------------------------------

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
  [[piece-type piece-amount board]]
  (do
    (loop [index 1]
      (do
        (eval `(assoc board ~(symbol (str (piece-type :name) index)) {:owner 1 :type '~piece-type}))
        (eval `(assoc board ~(symbol (str (piece-type :name) (+ 1 index))) {:owner -1 :type '~piece-type}))
        (if (< index piece-amount)
          (recur (+ 2 index)))))
    board))

(defn create-mass-pieces
  "Function to call create-pieces on a list of Type-Amount pairs, suitable for
   initializing the Board at start of game."
  [list-of-pairs board]
  (reduce create-pieces list-of-pairs board))

(defn initialize-pieces
  "This function creates all starting pieces at once, but they still have to be placed
   on the board."
  [board]
  (create-mass-pieces
   [[king-type 2]
    [rook-type 2]
    [bishop-type 2]
    [gold-general-type 4]
    [silver-general-type 4]
    [knight-type 4]
    [lance-type 4]
    [pawn-type 18]]
   board))

(defn pretty-print
  "DRAFT 2: Development method for pretty-printing the board.
  This one is adjusted for the (x y) coordinate lookup system."
  [board]
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

;; (defun declare-all
;;   ;;Function to declare 
;;   [board]
;;   (declare Lance1)
;;   (declare Lance2)
;;   (declare Lance3)
;;   (declare Lance4)
;;   (declare Knight1)
;;   (declare Knight2)
;;   (declare Knight3)
;;   (declare Knight4)
;;   (declare Bishop1)
;;   (declare Bishop2)
;;   (declare SilverGeneral1)
;;   (declare SilverGeneral2)
;;   (declare SilverGeneral3)
;;   (declare SilverGeneral4)
;;   (declare GoldGeneral1)
;;   (declare GoldGeneral2)
;;   (declare GoldGeneral3)
;;   (declare GoldGeneral4)
;;   (declare King1)
;;   (declare King2)
;;   (declare Rook1)
;;   (declare Rook2)
;;   (declare Pawn1)
;;   (declare Pawn2)
;;   (declare Pawn3)
;;   (declare Pawn4)
;;   (declare Pawn5)
;;   (declare Pawn6)
;;   (declare Pawn7)
;;   (declare Pawn8)
;;   (declare Pawn9)
;;   (declare Pawn10)
;;   (declare Pawn11)
;;   (declare Pawn12)
;;   (declare Pawn13)
;;   (declare Pawn14)
;;   (declare Pawn15)
;;   (declare Pawn16)
;;   (declare Pawn17)
;;   (declare Pawn18))

(defn setup-board
  "Function to set up the board for the initial gamestate.  Initializes all starting pieces,
   and sets up the board with each piece in its proper starting place.
   Also defines the board object and the game which containts that board.
   Note that these are all, essentially, global variables, but also all are
   ultimately contained, hierarchically, under the game map.

  Note that the piece names need never be used after declaration.  They are not any sort
  of important state, they're just a shorthand making it easier to declare everything.
  For debugging, you can use them to prettyprint, though.

  edit 22Dec2015: Attempting to make entirely functional.  Call it using: (setup-board {})"
  [game]
  ;;  (declare-all board)
  (assoc game
         :turn player1
         1 player2
         -1 player1
         player2 {:player 1 :hand hand2 :in-check? false}
         player1 {:player -1 :hand hand1 :in-check? false}
         hand2 []
         hand1 []
         :board (let [board (hash-map)]
           (initialize-pieces board)
           (assoc board
                  1 (sorted-map 1 Lance1 2 nil 3 Pawn1 4 nil 5 nil 6 nil 7 Pawn2
                                8 nil  9 Lance4)
                  2 (sorted-map 1 Knight1 2 Rook1 3 Pawn3 4 nil 5 nil 6 nil 7 Pawn4
                                8 Bishop2 9 Knight4)
                  3 (sorted-map 1 SilverGeneral1 2 nil 3 Pawn5 4 nil 5 nil 6 nil 7 Pawn6
                                8 nil  9 SilverGeneral4)
                  4 (sorted-map 1 GoldGeneral1 2 nil 3 Pawn7 4 nil 5 nil 6 nil 7 Pawn8
                                8 nil 9 GoldGeneral2)
                  5 (sorted-map 1 King1 2 nil 3 Pawn9 4 nil 5 nil 6 nil 7 Pawn10 8 nil
                                9 King2)
                  6 (sorted-map 1 GoldGeneral3 2 nil 3 Pawn11 4 nil 5 nil 6 nil 7 Pawn12
                                8 nil 9 GoldGeneral4)
                  7 (sorted-map 1 SilverGeneral3 2 nil 3 Pawn13 4 nil 5 nil 6 nil 7 Pawn14
                                8 nil 9 SilverGeneral4)
                  8 (sorted-map 1 Knight3 2 Bishop1 3 Pawn15 4 nil 5 nil 6 nil 7 Pawn16
                                8 Rook2 9 Knight4)
                  9 (sorted-map 1 Lance3 2 nil 3 Pawn17 4 nil 5 nil 6 nil 7 Pawn18
                                8 nil 9 Lance4)))))



;; ------------------Refactored  to purely functional up to here----------------------

(defn update-game
  "IMPORTANT: This function can be called at any time to update the game object to store the
   current state of the board, player-hands, turn counter, etc.
  THIS MUST BE CALLED AFTER EVERY TURN, before storing the game to JSON!"
  [board]
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
;; JSON:
;; *****************************************************************************************

;; As of now, the architecture looks like this:
;;     The client app: Stores multiple games in a local db, and in order to view or make a move
;;                            in a game, parses that into memory.  Only one game should ever
;;                            be loaded into memory at a time, so it's fine that that game
;;                            occupies global variable.
;;                     Transmits the game state after making a move to the server,
;;                            by simply reducing the game state to a json object,
;;                            and sending it on its way to be verified, stored, and passed
;;                            on to the other player.
;;                     Recieves new moves by way of a full json of the new game state,
;;                            and parses this into the native object format, such that moves can
;;                            be immediately made, or the game can be stored to the local DB for
;;                            later usage.
(defn board-to-json
  "Uses Cheshire's encode [== generate-string] to encode the state of the board to JSON"
  [board] (encode board))

(defn game-to-json
  "Uses Cheshire's encode [== generate-string] to encode the full game state to JSON"
  [board] (encode board))


(defn save-move [move-list from-x from-y to-x to-y promote]
  "Saves the most recent move to a movelist, which is itself expected to be directly portable to SQLite.
   NOTE: If a vector is used for move-list, JSON saving/recovery seems to be seamless."
  (conj move-list [from-x from-y to-x to-y promote]))

(defn move-list-to-string
  "Quick utility method for converting a movelist format: [[fromX fromY toX toY promoted?]]
   to a string which can be directly stored on older versions of SQLite."
  [move-list]
  (clojure.string/join ";" (map #(clojure.string/join "," %) move-list)))

;; TODO
;;   --------IMPORTANT------------
;;   -Set up Parse from JSON to game state.
;;   -----------------------------
;;  -





;; *****************************************************************************************
;; SQLite:
;; *****************************************************************************************

;; Provides access to the (TESTING) user database, which contains tables for:
;;          -User info
;;               >> UID (int), username (text), gamelist (int link),
;;               >> location (text), rank (int), friendlist (int link),
;;               >> providerID (int), client (enum, google/iOS)
;;               >> numGamesPlayed (int)
;;          -In-progress game list.
;;               >> GameID (int), Player1 (int, UID), Player2 (int, UID),
;;               >> GameState(JSON), CurrentTurn (int), DateOfLastMove(int),
;;
;;                  >> TODO: movelist
;;          TODO: -Ranked Play request list
;;               >> UID (int), username (text), rank (int), location (text)
;;          TODO: -Unranked Play request list
;;               >> UID (int), username (text), location (text), message (text)

(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "db/userdb"})



;;       TODO:
;; Create a player
;; Start new game
;; Send move
;; Receive move


;; UTILITY.  TO BE REMOVED.
(def testdata-sqlite
  {:UID 1112223
   :username "test1"
   :gamelist 1
   :location "mars"
   :rank 1
   :friendlist 2
   :providerID 123
   :client 1})

;; *****************************************************************************************
;; PostgreSQL:
;; *****************************************************************************************

;; Provides access to the (TESTING) user database, which contains tables for:
;;          -User info
;;               >> UID (int), username (text), gamelist (int link),
;;               >> location (text), rank (int), friendlist (int link),
;;               >> providerID (int), client (enum, google/iOS)
;;               >> numGamesPlayed (int)
;;          -In-progress game list.
;;               >> GameID (int), Player1 (int, UID), Player2 (int, UID),
;;               >> GameState(JSON), CurrentTurn (int), DateOfLastMove(int),
;;
;;                  >> TODO: movelist
;;          TODO: -Ranked Play request list
;;               >> UID (int), username (text), rank (int), location (text)
;;          TODO: -Unranked Play request list
;;               >> UID (int), username (text), location (text), message (text)


(def userdb
  {:classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname "//127.0.0.1:5432/userdb"})


;; UTILITY.  TO BE REMOVED.
(def testdata-psql
  {:UID 1112223
   :username "test1"
   :gamelist 1
   :location "mars"
   :rank 1
   :friendlist 2
   :providerID 123
   :client 1
   :gamesplayed 0})



;; *****************************************************************************************
;; Unit tests and supporting framework:
;; *****************************************************************************************

(defn board-fixture [function-in]
  (setup-board)
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


(use-fixtures :once board-fixture)

;; (deftest test-all
;;   (test-setup)
;;   (test-move-queries))
;;   (test-movement)
;;   (test-capture)
;;   (test-dropping)
;;   (test-check-detection))



;;*******************************************************************************
;;                       CURRENT NON-PURELY FUNCTIONAL OPERATIONS:
;;*******************************************************************************
;; pretty-printing
;; setting up the board.
;; Moving/dropping