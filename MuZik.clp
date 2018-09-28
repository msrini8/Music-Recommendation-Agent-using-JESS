(defglobal ?*name* = nil)
(defglobal ?*name* = nil)
(deftemplate user
    "User inputs"
    (slot genre)
    (slot album_type)
    (slot tf))

(deftemplate question
  (slot text)
  (slot type)
  (slot ident))

(deftemplate answer
  (slot ident)
  (slot answer))


(deffunction checking-in ()
    (printout t  crlf crlf crlf)
	(printout t "Hey, I my name is Mu Zik and I will help you discover new songs based on your interests." crlf)
	(printout t "Would you like to experiment with new music? Y/N" crlf)
	(bind ?agree (read) crlf)
	(if (or (eq ?agree Y)(eq ?agree y))then
    		(printout t "Awesome!Let's get to know you more.")
		(printout t " What is your name? " crlf)
		(bind ?name (read))
		(printout t " Hello, " ?name crlf)
		(bind ?*name* ?name)
        	(return TRUE))
	else (printout t "Come back later for more music recommendations." crlf)
    		(return FALSE))


(deffunction is-type (?answer ?type) 
    "Checking the answer has correct type"
    (if (eq ?type genres )then
        (return (or (eq ?answer HipHop)(eq ?answer Instrumental)(eq ?answer Metal)))      
    	  else (if(eq ?type ep-lp)then
                (return(or (or (eq ?answer Single)(eq ?answer Album))(or (eq ?answer Remix)(eq ?answer Cover))))
           	else(if (eq ?type time)then
        			(return(or (eq ?answer 80s)(eq ?answer 90s)(eq ?answer 2000s)(eq ?answer latest)))
                 )
         )
      )
 )

(deffunction ask-user(?question ?type)
    "Ask questiona and return answer"
    (bind ?answer "") 
    (while (not (is-type ?answer ?type))do
        (printout t ?question " " )
        (if (eq ?type genres )then
            (printout t  crlf)
            (printout t " HipHop or Instrumental or Metal "))
        (if (eq ?type ep-lp)then
            (printout t crlf "Single or Album or Remix or Cover" crlf ))
        (if (eq ?type time)then
            (printout t  crlf)
            (printout t crlf " 80s, 90s, 2000s, latest "))
        (bind ?answer (read))
     )        
    (return ?answer)
 )

(defrule request-genre
    (test (eq TRUE (checking-in)))
=>
(assert (ask genre)))


(defrule request-album_type
    (test (eq TRUE (checking-in)))
=>
(assert (ask album_type)))

(defrule request-tf
    (test (eq TRUE (checking-in)))
=>
(assert (ask tf)))


;rule that binds asserts the answer into the memeory
(defrule ask-question-by-id
 "Ask a question and assert the answer" 
(question (ident  ?id)(text ?text)(type ?type))
(not (answer (ident ?id)))
 ?ask <- (ask ?id)
 =>                        
 (bind ?answer (ask-user ?text ?type))
 (assert (answer (ident ?id)(answer ?answer)))
 (retract ?ask)
 (return)            
  )


(defrule HipHop-80s-Single 
  (user(album_type Single)(genre HipHop)(tf 80s))  
  =>       
 (printout t crlf "Here's your song recommendation:" 
        crlf "Song: The message"
        crlf "Artist: Grandmaster Flash and Furious Five" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
   ) 
(defrule HipHop-80s-Album
  (user(album_type Album)(genre HipHop)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Album: 3 Feet High and Rising"
        crlf "Artist: De La Soul" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
   ) 
(defrule HipHop-80s-Cover
  (user(album_type Cover)(genre HipHop)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: All things go"
        crlf "Artist: The bird and the bees" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-80s-Remix
  (user(album_type Remix)(genre HipHop)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Ghetto Brothers"
        crlf "Artist: Alan Taylor" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-90s-Single
  (user(album_type Single)(genre HipHop)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Regulate"
        crlf "Artist: Warren G." crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-90s-Album
  (user(album_type Album)(genre HipHop)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Illmatic"
        crlf "Artist: NAS" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-90s-Cover
  (user(album_type Cover)(genre HipHop)(tf 90s))  
  =>      
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: All eyes on me"
        crlf "Artist: 2PAC" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-90s-Remix
  (user(album_type Remix)(genre HipHop)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: All I need"
        crlf "Artist: Method Man" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-2000s-Single
  (user(album_type Single)(genre HipHop)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: P.S.A."
        crlf "Artist: Jay-Z" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-2000s-Album
  (user(album_type Album)(genre HipHop)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: The blueprint"
        crlf "Artist: Jay-Z" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-2000s-Cover
  (user(album_type Cover)(genre HipHop)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Chicken-n-Beer"
        crlf "Artist: Ludacris" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-2000s-Remix
  (user(album_type Remix)(genre HipHop)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Get by"
        crlf "Artist: Jay-Z and Kanye" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-latest-Single
  (user(album_type Single)(genre HipHop)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Congratulations"
        crlf "Artist: Post Malone" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-latest-Album
  (user(album_type Album)(genre HipHop)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: My dear melocholy"
        crlf "Artist: The Weeknd" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-latest-Remix
  (user(album_type Remix)(genre HipHop)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Aries"
        crlf "Artist: Rae Sremmurd" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule HipHop-latest-Cover
  (user(album_type Cover)(genre HipHop)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: 31 Days"
        crlf "Artist: Future" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )

(defrule Instrumental-80s-Single
  (user(album_type Single)(genre Instrumental)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Chariots of Fire"
        crlf "Artist: Vangelis" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-80s-Album
  (user(album_type Album)(genre Instrumental)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Peter Piper"
        crlf "Artist: Frank Mills" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-80s-Remix
  (user(album_type Remix)(genre Instrumental)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Peter Piper"
        crlf "Artist: Frank Mills" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )

(defrule Instrumental-80s-Cover
  (user(album_type Cover)(genre Instrumental)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Sirius"
        crlf "Artist: Alan Parson" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-90s-Single
  (user(album_type Single)(genre Instrumental)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Waterfalls"
        crlf "Artist: TLC" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-90s-Album
  (user(album_type Single)(genre Instrumental)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Here and Now"
        crlf "Artist: Ising" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-90s-Remix
  (user(album_type Remix)(genre Instrumental)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: High Energy"
        crlf "Artist: Evelyn Thomas" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-90s-Cover
  (user(album_type Cover)(genre Instrumental)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Breakout"
        crlf "Artist: Swing Sister" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-2000s-Single
  (user(album_type Single)(genre Instrumental)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Your hand in mine"
        crlf "Artist: Black Emperor" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-2000s-Album
  (user(album_type Album)(genre Instrumental)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: The mix-up"
        crlf "Artist: Bastie Boys" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-2000s-Remix
  (user(album_type Remix)(genre Instrumental)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Satan"
        crlf "Artist: Mogwai" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-2000s-Cover
  (user(album_type Cover)(genre Instrumental)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Tortoise"
        crlf "Artist: Djed" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-latest-Single
  (user(album_type Single)(genre Instrumental)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Love is blue"
        crlf "Artist: Paul Mauriat" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-latest-Album
  (user(album_type Album)(genre Instrumental)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: The hustle"
        crlf "Artist: Van McCoy" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-latest-Remix
  (user(album_type Remix)(genre Instrumental)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Rather Be"
        crlf "Artist: Clean Bandit and Jess Glynn" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Instrumental-latest-Cover
  (user(album_type Cover)(genre Instrumental)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Symphony"
        crlf "Artist: Clean Bandit and Zara Larson" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-80s-Single
  (user(album_type Single)(genre Metal)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Sweet child o' mine"
        crlf "Artist: Guns and Roses" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-80s-Album
  (user(album_type Album)(genre Metal)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Master of puppets"
        crlf "Artist: Metallica" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-80s-Remix
  (user(album_type Remix)(genre Metal)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Black metal"
        crlf "Artist: Venom" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-80s-Cover
  (user(album_type Cover)(genre Metal)(tf 80s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Black in black"
        crlf "Artist: ACDC" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-90s-Single
  (user(album_type Single)(genre Metal)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Sad but true"
        crlf "Artist: Metallica" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-90s-Album
  (user(album_type Album)(genre Metal)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Rust in peace"
        crlf "Artist: Megadeth" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-90s-Remix
  (user(album_type Remix)(genre Metal)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Motorhead"
        crlf "Artist: Motorhead" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-90s-Cover
  (user(album_type Cover)(genre Metal)(tf 90s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Leviathan"
        crlf "Artist: Mastodon" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-2000s-Single
  (user(album_type Single)(genre Metal)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: In the end"
        crlf "Artist: Linkin Park" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-2000s-Album
  (user(album_type Album)(genre Metal)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Iowa"
        crlf "Artist: Slipknot" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-2000s-Remix
  (user(album_type Remix)(genre Metal)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Numb"
        crlf "Artist: Linkin Park" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-2000s-Cover
  (user(album_type Cover)(genre Metal)(tf 2000s))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Chop-Suey!"
        crlf "Artist: System of a down" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-latest-Single
  (user(album_type Single)(genre Metal)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Moth into flame"
        crlf "Artist: Metallica" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-latest-Album
  (user(album_type Album)(genre Metal)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Code Orange"
        crlf "Artist: Forever" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-latest-Remix
  (user(album_type Remix)(genre Metal)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Goddamn Trouble"
        crlf "Artist: Overkill" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik")
    )
(defrule Metal-latest-Cover
  (user(album_type Cover)(genre Metal)(tf latest))  
  =>       
    (printout t crlf "Here's your song recommendation:" 
        crlf "Song: Knocking on heaven's doors"
        crlf "Artist: Guns 'n' Roses" crlf
        crlf "Enjoy your song! Hope you liked the music suggested by Mu Zik.")
    )
(defrule assert-user-fact
  (answer (ident genre)(answer ?gen))
  (answer (ident album_type)(answer ?alb))
  (answer (ident tf)(answer ?tf))        
    =>   
   (assert (user (genre ?gen)(album_type ?alb)(tf ?tf)))
 )


(deffacts question-data
"The questions the system can ask."
(question (ident genre) (type genres)
(text " What genre of music do you feel like listening to? " ))
(question (ident album_type) (type ep-lp)
(text " What type of an album would you prefer? "))  
(question (ident tf) (type time)
(text " Which generation music do you prefer? ")))

(reset)
(run)
