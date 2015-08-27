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
  (:gen-class))

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

(def row (sorted-map 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil))
(def board (sorted-map 1 row 2 row 3 row 4 row 5 row 6 row 7 row 8 row 9 row))

(defn create-pieces
  "Function to instantiate Piece Types.  Creates the specified number of Pieces, of
   the specified Type, distributed evenly between the two players' ownership."
  [[type amount]]
  (loop [index 1]
    (do
      (eval `(def ~(symbol (str (get type :name) index)) {:owner -1 :type type}))
      (eval `(def ~(symbol (str (get type :name) (inc index))) {:owner 1 :type type}))
      (if (< index amount)
        (recur (+ 2 index))))))

(defn create-mass-pieces
  "Function to call create-pieces on a list of Type-Amount pairs, suitable for
   initializing the Board at start of game."
  [list-of-pairs]
  (map create-pieces list-of-pairs))



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

(defn move-single-space [origin-x origin-y player]
  "Utility function, primarily for move-jump, to apply the tests for move legality to a single
   space, rather than a series of spaces."
  (if (is-on-board origin-x origin-y)
    (if (= (get-in board [origin-y origin-x]) nil)
      [origin-x origin-y]
      (if (= (get (get-in board [origin-y origin-x]) :owner) player)
        []
        [origin-x origin-y]))
    []))

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
    (if (<= 0 remaining)
      ;; If the current space is unoccupied, then proceed on this direction, else
      ;;    check if it's player's or opponent's and handle:
      (if (= (get-in board [origin-y origin-x]) nil)
        (let [further-moves  (move-direction
                               [direction-x direction-y
                                (+ direction-x origin-x) (+ direction-y origin-y)
                                (- remaining 1) player])]
             (into further-moves [[origin-x origin-y]]))
        ;; If the space is occupied by Player's Piece, stop checking this direction, else
        ;; handle as either capturable Piece or Check/Checkmate
        (if (= (get (get-in board [origin-y origin-x]) :owner) player)
          []
          ;; (TODO: )If the opponent's piece is a King, indicate Check
          ;;        (TODO: CHECK CHECKMATE),
          ;; else simply indicate it as a valid move and stop checking this direction
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
    (remove #(= % [origin-x origin-y]) (into (move-direction [1 0 origin-x origin-y
                                                             spaces player])
                                            available-moves))))

(defn move-diagonal
  "Function for handling the fact that this piece can move in 4 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (let [available-moves-left (into (move-direction [-1 -1 origin-x origin-y spaces player])
                            (move-direction [-1 1 origin-x origin-y spaces player]))
        available-moves-right (into (move-direction [1 -1 origin-x origin-y spaces player])
                                       (move-direction [1 1 origin-x origin-y spaces player]))]
  (remove #(= % [origin-x origin-y]) (into available-moves-left available-moves-right))))

(defn move-diagonal-forward
  "Function for handling the fact that this piece can move in 2 diagonal directions,
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (let [available-moves-left (move-direction [-1 1 origin-x origin-y spaces player])
        available-moves-right (move-direction [1 1 origin-x origin-y spaces player])]
    (remove #(= % [origin-x origin-y]) (into available-moves-left available-moves-right))))


(defn move-forward
  "Function for handling the fact that this piece can move forward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
    (remove #(= % [origin-x origin-y]) (move-direction [0 (* player 1) origin-x origin-y
                                                              spaces player])))

(defn move-backward
  "Function for handling the fact that this piece can move backward (relative to owner),
   with single-space or full-board movement determined by the value of the spaces variable
   Returns a list of available moves, or an empty vector if none exist."
  [spaces origin-x origin-y player]
  (remove #(= % [origin-x origin-y]) (move-direction [0 (* player -1) origin-x origin-y
                                                      spaces])))

(defn move-jump
  "Function for handling Knights' ability to move by jumping to spaces.
   Mimics move-direction, but simplified for the situation.
   NOTE: the spaces parameter is kept, but not used, to ease generic movement-function calling.
   Returns a list of available jumps, or an empty vector if none are available."
  [_ origin-x origin-y player]
  (let [accumulator (vector (move-single-space (+ origin-x 1) (+ origin-y (* 2 player)) player))]
    (remove #(= % [origin-x origin-y])
            (into accumulator (vector (move-single-space (- origin-x 1)
                                                         (+ origin-y (* 2 player)) player))))))





;; *****************************************************************************************
;; Board Setup:
;; *****************************************************************************************


;; Type Definition and Initialization:
;; ---------------------------------------------

(def king-type {:moves [[move-horizontal 1]
                        [move-forward 1]
                        [move-backward 1]
                        [move-diagonal 1]]
                :is-promotable false
                :promotion `king-type
                :name "King"})

(def rook-type {:moves [[move-horizontal 9]
                        [move-forward 9]
                        [move-backward 9]]
                :is-promotable true
                :promotion `promoted-rook-type
                :name "Rook"})

(def promoted-rook-type {:moves [[move-horizontal 1]
                                 [move-forward 1]
                                 [move-backward 1]
                                 [move-diagonal 9]]
                :is-promotable false
                :promotion `rook-type
                :name "PromotedRook"})

(def bishop-type {:moves [[move-diagonal 9]]
                :is-promotable true
                :promotion `promoted-bishop-type
                :name "Bishop"})

(def promoted-bishop-type {:moves [[move-horizontal 1]
                                   [move-forward 1]
                                   [move-backward 1]
                                   [move-diagonal 9]]
                :is-promotable false
                :promotion `bishop-type
                :name "PromotedBishop"})

(def gold-general-type {:moves [[move-horizontal 1]
                                [move-forward 1]
                                [move-backward 1]
                                [move-diagonal-forward 1]]
                :is-promotable false
                :promotion `gold-general-type
                :name "GoldGeneral"})

(def silver-general-type {:moves [[move-forward 1]
                                  [move-diagonal 1]]
                :is-promotable true
                :promotion `promoted-silver-general-type
                :name "SilverGeneral"})

(def promoted-silver-general-type {:moves [[move-horizontal 1]
                                           [move-forward 1]
                                           [move-backward 1]
                                           [move-diagonal-forward 1]]
                :is-promotable false
                :promotion `silver-general-type
                :name "PromotedSilverGeneral"})

(def knight-type {:moves [[move-jump 1]]
                :is-promotable true
                :promotion `promoted-knight-type
                :name "Knight"})

(def promoted-knight-type {:moves [[move-horizontal 1]
                                   [move-forward 1]
                                   [move-backward 1]
                                   [move-diagonal-forward 1]]
                :is-promotable false
                :promotion `knight-type
                :name "PromotedKnight"})

(def lance-type {:moves [[move-forward 9]]
                :is-promotable true
                :promotion `promoted-lance-type
                :name "Lance"})

(def promoted-lance-type {:moves [[move-horizontal 1]
                                  [move-forward 1]
                                  [move-backward 1]
                                  [move-diagonal-forward 1]]
                :is-promotable false
                :promotion `knight-type
                :name "PromotedKnight"})

(def pawn-type {:moves [[move-forward 1]]
                :is-promotable true
                :promotion `promoted-pawn-type
                :name "Pawn"})

(def promoted-pawn-type {:moves [[move-horizontal 1]
                                 [move-forward 1]
                                 [move-backward 1]
                                 [move-diagonal-forward 1]]
                :is-promotable false
                :promotion `pawn-type
                :name "PromotedPawn"})



;; Type Definition and Initialization:
;; ---------------------------------------------

(defn initialize-pieces
  "This function creates all starting pieces."
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


;; Player Definition and Initialization:
;; ---------------------------------------------

(def player1 {:player -1})
(def player2 {:player 1})
(def hand1 {})
(def hand2 {})
(def turn player1)



;; Board Definition and Initialization:
;; ---------------------------------------------

(def row1 (sorted-map 1 Lance4 2 Knight4 3 SilverGeneral4 4 GoldGeneral4
                      5 King2 6 GoldGeneral2 7 SilverGeneral2 8 Knight2 9 Lance2))
(def row2 (sorted-map 1 nil 2 Rook2 3 nil 4 nil 5 nil 6 nil 7 nil 8 Bishop2 9 nil))
(def row3 (sorted-map 1 Pawn10 2 Pawn12 3 Pawn14 4 Pawn16 5 Pawn18 6 Pawn2 7 Pawn4
                      8 Pawn6 9 Pawn8))
(def row4 (sorted-map 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil))
(def row5 (sorted-map 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil))
(def row6 (sorted-map 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil 9 nil))
(def row7 (sorted-map 1 Pawn1 2 Pawn3 3 Pawn5 4 Pawn7 5 Pawn9 6 Pawn11 7 Pawn13
                      8 Pawn15 9 Pawn17))
(def row8 (sorted-map 1 nil 2 Rook1 3 nil 4 nil 5 nil 6 nil 7 nil 8 Bishop1 9 nil))
(def row9 (sorted-map 1 Lance1 2 Knight1 3 SilverGeneral1 4 GoldGeneral1
                      5 King1 6 GoldGeneral3 7 SilverGeneral3 8 Knight3 9 Lance3))


(def board (sorted-map 1 row1 2 row2 3 row3 4 row4 5 row5 6 row6 7 row7 8 row8 9 row9
                       :player1 player1 :player2 player2 :hand1 hand1 :hand2 hand2))


(def game (hash-map :board board :turn turn))
