(ns push307.core
  (:gen-class))

(import com.zetcode.SpaceInvaders)
(import java.awt.EventQueue)

(defn run-me
  []
  (def game (SpaceInvaders.))
  (.setVisible game true))

(defn run
  []
  (EventQueue/invokeLater run-me))