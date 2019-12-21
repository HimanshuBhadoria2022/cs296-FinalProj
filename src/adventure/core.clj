(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:refer-clojure :exclude [take drop])
(:gen-class))

(def init-map
  {:kitchen {
      :desc "Here you can find a knife ('take knife').\nUse 'n' to go to garden, 's' to go to closet, 'w' to go to garage, 'e' to go to bedroom."
      :title "the kitchen"
      :dir {:north :garden
            :west :garage
            :south :closet
            :east :bedroom}
      :contents #{:knife}}
   :garden {
      :desc "Here you can find spinach ('take spinach').\nUse 'e' to go to apple tree, 's' to go to kitchen, 'w' to go to shed."
      :title "the garden"
      :dir {:east :tree
            :south :kitchen
            :west :shed}
      :contents #{:spinach}}
    :tree {
      :desc "Here you can get apple ('take apple').\nUse 'w' to go to garden, 'e' to visit the barn."
      :title "the apple tree"
      :dir {:west :garden
            :east :barn}
      :contents #{:apple}}
    :shed {
      :desc "Here you can get milk ('take milk').\nUse 'e' to go to garden, 'w' to visit the fish pond."
      :title "the shed"
      :dir {:east :garden
            :west :pond}
      :contents #{:milk}}
    :barn {
      :desc "You visit the horses. Your happiness level just increased!\nUse 'w' to go to the apple tree."
      :title "the barn"
      :dir {:west :tree}
      :contents #{}}
    :garage {
      :desc "Here is the blender ('take blender').\nUse 'e' to go to kitchen."
      :title "the garage"
      :dir {:east :kitchen}
      :contents #{:blender}}
    :closet {
      :desc "Here is where the sugar is stored ('take sugar').\nUse 'n' to return to kitchen."
      :title "the closet"
      :dir {:north :kitchen}
      :contents #{:sugar}}
    :bedroom {
      :desc "Looks like someone is asleep in the bedroon.\nUse 'w' to exit quietly."
      :title "the bedroom"
      :dir {:west :kitchen}
      :contents #{}}
    :pond {
      :desc "Here is a beautiful fish pond. You happiness level just increased!.\nUse 'e' to return to the shed."
      :title "the fish pond"
      :dir {:east :shed}
      :contents #{}}
  }
)

(def init-items
  {:knife {
     :desc "A knife for cutting an 'apple' with 'slice'."
     :title "a knife"}
   :spinach {
     :desc "Spinach for your smoothie."
     :title "spinach"}
   :apple {
     :desc "An apple that you must 'slice' with a 'knife'."
     :title "an apple"}
   :sliced-apple {
     :desc "A slice apple is better for smoothie making."
     :title "a sliced apple"}
   :milk {
     :desc "Some milk for your smoothie."
     :title "milk"}
   :smoothie {
     :desc "You did it! Use 'drink' to consume!"
     :title "smoothie"}
   :blender {
     :desc "If you have the ingredients, use the blender to 'blend'."
     :title "blender"}
   :sugar {
     :desc "You need some sugar for the smoothie."
     :title "sugar"}
  }
)

(def init-adventurer {
  :location :kitchen
  :inventory #{}
  :happiness 0
  :weight 0
  :tick 0
  :seen #{}
  }
)

(defn ticktick [player]
  (update-in player [:tick] inc))

(defn incWeight [player]
  (update-in player [:weight] inc))

(defn decWeight [player]
  (update-in player [:tick] dec))

(defn incHappiness [dest player]
  (let [idx (player :happiness)]
    (if(or (= dest :barn) (= dest :pond))
      (do (printf "\nHappiness increased to %s!" (+ idx 1))
          (update-in player [:happiness] inc))
      player)
  )
)

(defn go [dir player]
    (let [location (player :location)
          dest (->> init-map location :dir dir)]
      (if (nil? dest)
        (do (println "You can't go that way.") player)
        (assoc-in (incHappiness dest player) [:location] dest))
    )
)

(defn status [player]
  (let [location (player :location)]
    (print (str "\nYou are in " (-> init-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> init-map location :desc)))
    (update-in player [:seen] #(conj % location))
  )
)

(defn take [item player]
	(let [location (player :location)
			content (-> init-map location :contents)
			title (-> init-items item :title)
			desc (-> init-items item :desc)
			inv (-> player :inventory)]
			(if (contains? content item)
				(if (contains? inv item)
					(do (printf "\nYou already have it.\n") player)
					(do (printf "\nYou picked up %s. %s\n" title desc)(update-in (incWeight player) [:inventory] #(conj % item))))
				(do (println "That's not here.\n") player))))

(defn drop [item player]
  (let [i (player :inventory)
        title (-> init-items item :title)]
    (if (contains? i item)
      (do (printf "\nDropped %s.\n" title) (update-in (decWeight player) [:inventory] #(disj % item)))
      (do (printf "\nYou don't have %s\n" title) player)
    )
  ))

(defn examine [item player]
  (let [title (-> init-items item :title)
        desc (-> init-items item :desc)
        inv (-> player :inventory)]
      (if (contains? inv item)
        (do (printf "\nTitle: %s. \nDescription: %s\n" title desc) player)
        (do (println "\nYou don't have that item.") player)
      )
  ))

(defn slice [item1 item2 item3 player]
  (let [title1 (-> init-items item1 :title)
        title2 (-> init-items item2 :title)
        inv (-> player :inventory)]
      (if (contains? inv item1)
        (if (contains? inv item2)
          (if (contains? inv item3)
            (do (println "\nYou already have a sliced apple.") player)
            (do (printf "\nNow you have a sliced apple.\n")(update-in (update-in player [:inventory] #(disj % item2)) [:inventory] #(conj % item3)))
          )
          (do (println "\nYou need an apple from the apple tree.") player)
        )
        (do (println "\nYou need a knife from the kitchen.\n") player)
      )
  ))

(defn blend [item1 item2 item3 item4 item5 item6 player]
  (let [title1 (-> init-items item1 :title)
        title2 (-> init-items item2 :title)
        title3 (-> init-items item3 :title)
        title4 (-> init-items item4 :title)
        title5 (-> init-items item5 :title)
        title6 (-> init-items item6 :title)
        inv (-> player :inventory)]
      (if (and (contains? inv item1) (contains? inv item2)
                (contains? inv item3) (contains? inv item4)
                (contains? inv item5))
        (do (println "\nYou made a smoothie! Now, 'drink' it to complete the game!") (update-in player [:inventory] #(conj % item6)))
        (do (println "\nYou don't have all the ingredients.") player)
      )
  ))

(defn drink [item player]
  (let [inv (-> player :inventory)]
    (if (contains? inv item)
      (do (println "\nCongrats, game complete! Use 'quit' to exit.") player)
      (do (println "\nYou don't have a smoothie to drink.") player)
    )
  )
)

(defn seeitems [player]
  (let [i (player :inventory)]
    (if (empty? i)
      (do (println "\nNothing in inventory.") player)
      (do (printf "\nInventory: %s\n" i) player))))

(defn help [player]
  (do (printf "\nGoal is to gather ingredients and make smoothie.\nInstructions:\nn, s, e, w\nlook\nexamine (item), take (item), drop (item)\ni, inventory\nslice, blend, drink\nquit\n") player)
)

(defn respond [player command]
  (match command
    [:look] (update-in player [:seen] #(disj % (-> player :location)))
    [:help] (help player)
    [(:or :north :n)] (go :north (ticktick player))
    [(:or :south :s)] (go :south (ticktick player))
    [(:or :east :e)] (go :east (ticktick player))
    [(:or :west :w)] (go :west (ticktick player))
    [:take :knife] (take :knife (ticktick player))
    [:take :spinach] (take :spinach (ticktick player))
    [:take :apple] (take :apple (ticktick player))
    [:take :milk] (take :milk (ticktick player))
    [:take :sugar] (take :sugar (ticktick player))
    [:take :blender] (take :blender (ticktick player))
    [:examine :knife] (examine :knife (ticktick player))
    [:examine :spinach] (examine :spinach (ticktick player))
    [:examine :apple] (examine :apple (ticktick player))
    [:examine :milk] (examine :milk (ticktick player))
    [:examine :sugar] (examine :sugar (ticktick player))
    [:examine :blender] (examine :blender (ticktick player))
    [:examine :sliced :apple] (examine :sliced-apple (ticktick player))
    [:examine :smoothie] (examine :smoothie (ticktick player))
    [:drop :knife] (drop :knife (ticktick player))
    [:drop :spinach] (drop :spinach (ticktick player))
    [:drop :apple] (drop :apple (ticktick player))
    [:drop :milk] (drop :milk (ticktick player))
    [:drop :sugar] (drop :sugar (ticktick player))
    [:drop :blender] (drop :blender (ticktick player))
    [:drop :smoothie] (drop :smoothie (ticktick player))
    [:drop :sliced :apple] (drop :sliced-apple (ticktick player))
    [:drop :sliced-apple] (drop :sliced-apple (ticktick player))
    [(:or :i :inventory)] (seeitems (ticktick player))
    [:slice] (slice :knife :apple :sliced-apple (ticktick player))
    [:blend] (blend :blender :sugar :spinach :sliced-apple :milk :smoothie (ticktick player))
    [:drink] (drink :smoothie (ticktick player))
    [:quit] (System/exit 0)
    _ (do (println "\nNot a valid instruction.") player)
  )
)

(defn canonicalize [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [my-map init-map
        my-player init-adventurer]
    (let [pl (status my-player)
        _  (println "\nWhat do you want to do? ('look' and 'help' for info)")
        command (read-line)]
        (recur my-map (respond pl (canonicalize command))))))
