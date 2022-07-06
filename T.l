#{

  TODO:
  - refactor length calcs
  - memoization/cache?

}#

# bonuses
(def '+Sequential-Bonus   15) # bonus for adjacent Matches
(def '+Separator-Bonus    30) # bonus if match occurs after a separator
(def '+Camel-Bonus        30) # bonus if match is uppercase and prev is lower
(def '+First-Letter-Bonus 15) # bonus if the first letter is matched

# penalty
(def '+Leading-Letter-Penalty     -5)  # penalty applied for every letter in Str before the first match
(def '+Max-Leading-Letter-Penalty -15) # maximum penalty for leading letters
(def '+Unmatched-Letter-Penalty   -1)  # penalty for every letter that doesn't matter

(def '*Recursion-Count 0)

(de fuzzy-match-simple (Pattern Str) # str ,str -> bool
  (let (Pattern-Idx 1
        Str-Idx 1
        Pattern-Length (length Pattern)
        Str-Length (length Str))

    (while 
      (and
        (<> Pattern-Idx Pattern-Length)
        (<> Str-Idx Str-Length))
      (setq
        Pattern-Char (lowc (get Pattern Pattern-Idx)) # Pattern[Pattern-Idx]
        Str-Char (lowc (get Str Str-Idx))) # Str[Str-Idx]
      (when (= Pattern-Char Str-Char) (inc 'Pattern-Idx))
      (inc 'Str-Idx))

    (and
      (n0 Pattern-Length)
      (n0 Str-Length)
      (= Pattern-Idx Pattern-Length))))

# public function
(de fuzzy-match-go (Pattern Str) # str str out-int -> bool
  (let Matches (need 256 0) # new byte[256];
    (fuzzy-match 
      (chop Pattern) (chop Str) Matches (length Matches))))

(de fuzzy-match (Pattern Str Matches Max-Matches) # bool
  (let (Recursion-Count 0
        Recursion-Limit 10)
    (fuzzy-match-recursive
      Pattern Str 1 1 
      NIL Matches Max-Matches 1
      Recursion-Count Recursion-Limit)))

(de block-copy (Src Off-Src Dst Off-Dst Cnt)
  # copy or splice list in
  (do Cnt
    (set Dst (car Src))
    (shift 'Dst)
    (shift 'Src)))  
      
## Private implementation -> bool
(de fuzzy-match-recursive
  (Pattern 
   Str
   Pattern-Cur-Index
   Str-Cur-Index
   Src-Matches
   Matches
   Max-Matches
   Next-Match
   Recursion-Count
   Recursion-Limit)

  ## Count recursions
  (inc '*Recursion-Count)
  
  (when *Dbg (prinl) (println '*Recursion-Count *Recursion-Count))
  
  ## Detect end of Strings
  (when
    (and
      (< *Recursion-Count Recursion-Limit)
      (<= Pattern-Cur-Index (length Pattern))
      (<= Str-Cur-Index (length Str)))
    
    ## Recursion params
    (let (Recursive-Match NIL # bool
          Best-Recursive-Matches (need 256 0)
          Best-Recursive-Score 0 # int
          First-Match T) # bool

      ## Loop through Pattern and Str looking for a match
      (while 
        (and
          (<= Pattern-Cur-Index (length Pattern))
          (<= Str-Cur-Index (length Str)))
          
        ## Found match
        (when 
          (= 
            (lowc (get Pattern Pattern-Cur-Index)) 
            (lowc (get Str Str-Cur-Index)))
            
          (when *Dbg (println 'lowc (dec Pattern-Cur-Index) (dec Str-Cur-Index)))
            
          ## Supplied Matches buffer was too short
          (when (>= Next-Match Max-Matches)
            (throw 'return NIL))

          ## "Copy-on-Write" Src-Matches into Matches
          # (First-Match && Src-Matches != null)
          (when (and First-Match Src-Matches)
            (when *Dbg 
              (println 'copy-matches Next-Match)
              (println Matches))
            (block-copy Src-Matches 0 Matches 0 Next-Match)
            (when *Dbg
              (println Matches))
            (setq First-Match NIL))

          ## Recursive call that "skips" this match
          (let Recursive-Matches (need 256 0)
            (let? Recursive-Score
                    (fuzzy-match-recursive
                      Pattern 
                      Str 
                      Pattern-Cur-Index 
                      (+ Str-Cur-Index 1)
                      Matches 
                      Recursive-Matches 
                      (length Recursive-Matches) 
                      Next-Match
                      *Recursion-Count
                      Recursion-Limit)

              ## Pick best recursive score
              (when 
                (or 
                  (not Recursive-Match)
                  (> Recursive-Score Best-Recursive-Score))
                (block-copy Recursive-Matches 0 Best-Recursive-Matches 0 256)
                (setq Best-Recursive-Score Recursive-Score))

              (setq Recursive-Match T)))

          ## Advance
          (set (nth Matches Next-Match) Str-Cur-Index) (inc 'Next-Match)
          (inc 'Pattern-Cur-Index)
          (when *Dbg
            (println 'update-matches Next-Match Str-Cur-Index)
            (println Matches)))
        
        (inc 'Str-Cur-Index)) # while

      ## Determine if full Pattern was matched
      (setq Matched
        (> Pattern-Cur-Index (length Pattern))) # bool

      ## Initialize score
      (let Out-Score 100     
          
        ## Calculate score
        (when Matched

          ## Apply leading letter Penalty
          (let Penalty
                (* +Leading-Letter-Penalty 
                  (dec (car Matches)))
            (when (< Penalty +Max-Leading-Letter-Penalty)
              (setq Penalty +Max-Leading-Letter-Penalty))
            (inc 'Out-Score Penalty))
          (when *Dbg (println 'out-score Out-Score (dec (car Matches))))
          
          ## Iterate Str to end
          ## Apply Unmatched Penalty
          (inc 'Out-Score 
            (* +Unmatched-Letter-Penalty
              (inc (- (length Str) Next-Match))))
          (when *Dbg (println 'out-score Out-Score))
              
          ## Apply ordering bonuses
          (when *Dbg (println 'next-match Next-Match))
          (for (I 1 (< I Next-Match) (inc I))
          
            (let Curr-Idx (get Matches I)

              (when (> I 1)
                (let Prev-Idx (get Matches (- I 1))
                  ## Sequential
                  (when (= Curr-Idx (+ Prev-Idx 1))
                    (inc 'Out-Score +Sequential-Bonus))))

              ## Check for bonuses based on neighbor character value
              (if (> Curr-Idx 1)

                  ## Camel case
                  (let (Neighbor (get Str (- Curr-Idx 1))
                        Curr (get Str Curr-Idx))

                    (when (and (low? Neighbor) (upp? Curr))
                      (inc 'Out-Score +Camel-Bonus))

                    ## Separator
                    (when
                      (or
                        (= Neighbor "_") 
                        (= Neighbor " ")
                        (format Neighbor))
                      (inc 'Out-Score +Separator-Bonus)))

                  # else

                  ## First letter
                  (inc 'Out-Score +First-Letter-Bonus))))) # when

        (when *Dbg (println Recursive-Match Matched Out-Score))
        
        ## Return best result
        (cond
          ((and 
              Recursive-Match 
              (or 
                (not Matched)
                (> Best-Recursive-Score Out-Score)))

            ## Recursive score is better than "this"
            (block-copy Best-Recursive-Matches 0 Matches 0 Max-Matches)
            Best-Recursive-Score)

          ## "this" score is better than recursive          
          (Matched Out-Score)
            
          ## no match
          (T))))))


# test each line
(de main ()
  (in "data"
    (let Pat "LLL"
      (use Line
        (while (setq Line (line)) # list
          (setq *Recursion-Count 0)
          (let ((Score-Valid Str) (mapcar '((X) (pack X)) (split Line "|"))
                Score (let println println (format (fuzzy-match-go Pat Str))))
            (println 'T= Score-Valid 'Score= Score Str) #(bye)
            )))))
  (bye))
  
